#!/usr/bin/env rust-script
//! ```cargo
//! [dependencies]
//! clap = { version = "4", features = ["derive"] }
//! clap_complete = "4"
//! anyhow = "1"
//! sha256 = "1"
//! regex = "1"
//! serde_json = "1"
//! ```

use std::{
    fs,
    io::{IsTerminal, Write},
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::Shell;
use regex::Regex;
use serde_json::{Map, Value};
use sha256::digest;

/// Application-specific metadata box type. Private by design: must not start
/// with "jxl". The payload is UTF-8 JSON.
const JSON_BOX: &[u8; 4] = b"json";

/// println!, but a broken pipe (`jxlmeta list | head`) is not an error.
macro_rules! outln {
    ($($t:tt)*) => {{
        let _ = writeln!(std::io::stdout(), $($t)*);
    }};
}

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Attach a JSON metadata box to a JPEG XL container (in place)
    Attach {
        /// JPEG XL container file
        image: PathBuf,
        /// UTF-8 JSON file to embed
        meta: PathBuf,
    },
    /// Print the JSON metadata box of a JPEG XL file
    Get {
        /// JPEG XL container file
        image: PathBuf,
    },
    /// Set one or more keys in the JSON metadata box (in place)
    Set {
        /// JPEG XL container file
        image: PathBuf,
        /// Assignments, e.g. model=flux seed=42
        #[arg(value_name = "KEY=VALUE", required = true)]
        pairs: Vec<String>,
    },
    /// Remove the JSON metadata box from a JPEG XL file (in place)
    Rm {
        /// JPEG XL container file
        image: PathBuf,
    },
    /// List files carrying a JSON metadata box (default: *.jxl)
    List {
        /// JPEG XL files (default: *.jxl)
        files: Vec<PathBuf>,
    },
    /// Check sha256-named *.jxl files for hash mismatches
    Lint,
    /// Generate shell completions
    Completions {
        /// The shell to generate completions for
        #[arg(value_enum)]
        shell: Shell,
    },
}

struct BoxEntry {
    ty: [u8; 4],
    payload: Vec<u8>,
}

/// True if the bytes are a JPEG XL container (signature box), not a bare
/// codestream. Only containers can carry metadata boxes.
fn is_container(data: &[u8]) -> bool {
    data.len() >= 12 && data[..4] == [0, 0, 0, 0x0c] && &data[4..8] == b"JXL "
}

/// Iterate top-level ISO-BMFF boxes, yielding (offset, size, type).
/// Handles 32-bit size, 64-bit extended size (size field == 1), and
/// "extends to EOF" (size field == 0).
fn walk_boxes(data: &[u8]) -> Result<Vec<(usize, usize, [u8; 4])>> {
    let mut out = Vec::new();
    let mut pos = 0usize;

    while pos + 8 <= data.len() {
        let size32 = u32::from_be_bytes(data[pos..pos + 4].try_into().unwrap());
        let mut ty = [0u8; 4];
        ty.copy_from_slice(&data[pos + 4..pos + 8]);

        let (size, header) = match size32 {
            1 => {
                if pos + 16 > data.len() {
                    bail!("truncated 64-bit box header at offset {pos}");
                }
                let s = u64::from_be_bytes(data[pos + 8..pos + 16].try_into().unwrap());
                (usize::try_from(s).context("box size overflows usize")?, 16usize)
            }
            0 => (data.len() - pos, 8usize),
            n => (n as usize, 8usize),
        };

        if size < header || pos + size > data.len() {
            bail!("invalid box size {size} at offset {pos}");
        }
        out.push((pos, size, ty));
        pos += size;
    }

    if pos != data.len() {
        bail!("trailing {} bytes after last box", data.len() - pos);
    }
    Ok(out)
}

fn read_boxes(data: &[u8]) -> Result<Vec<BoxEntry>> {
    walk_boxes(data)?
        .into_iter()
        .map(|(start, size, ty)| {
            let header = if data[start..start + 4] == 1u32.to_be_bytes() {
                16
            } else {
                8
            };
            Ok(BoxEntry {
                ty,
                payload: data[start + header..start + size].to_vec(),
            })
        })
        .collect()
}

/// Byte range of the JSON box within the file, if present.
fn json_box_range(data: &[u8]) -> Result<Option<(usize, usize)>> {
    Ok(walk_boxes(data)?
        .into_iter()
        .find(|(_, _, ty)| *ty == *JSON_BOX)
        .map(|(start, size, _)| (start, size)))
}

/// Encode a box. Uses a 64-bit extended size when the box would exceed the
/// 32-bit size field.
fn encode_box(ty: &[u8; 4], payload: &[u8]) -> Vec<u8> {
    let total = 8u64 + payload.len() as u64;
    let mut v = Vec::with_capacity(total as usize + 8);

    if total < (1u64 << 32) {
        v.extend_from_slice(&(total as u32).to_be_bytes());
        v.extend_from_slice(ty);
    } else {
        v.extend_from_slice(&1u32.to_be_bytes());
        v.extend_from_slice(ty);
        v.extend_from_slice(&(total + 8).to_be_bytes());
    }
    v.extend_from_slice(payload);
    v
}

fn find_json_box(data: &[u8]) -> Result<Option<Vec<u8>>> {
    Ok(read_boxes(data)?
        .into_iter()
        .find(|b| b.ty == *JSON_BOX)
        .map(|b| b.payload))
}

/// Read the file, require a container, and require the signature to be sane.
fn load_container(image: &Path) -> Result<Vec<u8>> {
    let data = fs::read(image).with_context(|| format!("read {}", image.display()))?;
    if !is_container(&data) {
        bail!(
            "{} is not a JPEG XL container; metadata boxes need the container format",
            image.display()
        );
    }
    Ok(data)
}

/// Atomic in-place replace: write to a sibling temp file, then rename.
fn replace_file(image: &Path, bytes: &[u8]) -> Result<()> {
    let tmp = image.with_extension("jxl.tmp");
    fs::write(&tmp, bytes).with_context(|| format!("write {}", tmp.display()))?;
    fs::rename(&tmp, image).with_context(|| format!("rename into {}", image.display()))?;
    Ok(())
}

fn cmd_attach(image: &Path, meta: &Path) -> Result<()> {
    let data = load_container(image)?;
    let payload = fs::read(meta).with_context(|| format!("read {}", meta.display()))?;
    serde_json::from_slice::<Value>(&payload)
        .with_context(|| format!("{} is not valid JSON", meta.display()))?;

    if json_box_range(&data)?.is_some() {
        bail!("{} already has a \"json\" box", image.display());
    }

    let mut out = data;
    out.extend_from_slice(&encode_box(JSON_BOX, &payload));
    replace_file(image, &out)?;

    outln!("attached {} bytes of JSON to {}", payload.len(), image.display());
    Ok(())
}

fn cmd_get(image: &Path) -> Result<()> {
    let data = fs::read(image).with_context(|| format!("read {}", image.display()))?;
    let payload = find_json_box(&data)
        .with_context(|| format!("parse {}", image.display()))?
        .with_context(|| format!("no \"json\" box in {}", image.display()))?;

    let mut stdout = std::io::stdout();
    // tty -> YAML, pipe -> the raw JSON box bytes.
    if stdout.is_terminal() {
        match serde_json::from_slice::<Value>(&payload) {
            Ok(v) => {
                let mut s = String::new();
                yaml_emit(&v, 0, &mut s);
                stdout.write_all(s.as_bytes())?;
            }
            Err(_) => {
                stdout.write_all(&payload)?;
                stdout.write_all(b"\n")?;
            }
        }
    } else {
        stdout.write_all(&payload)?;
        stdout.write_all(b"\n")?;
    }
    Ok(())
}

/// Parse a `KEY=VALUE` assignment. The value is parsed as JSON when possible
/// (`42`, `true`, `[1,2]`), otherwise treated as a string.
fn parse_assignment(pair: &str) -> Result<(String, Value)> {
    let (key, raw) = pair
        .split_once('=')
        .with_context(|| format!("expected KEY=VALUE, got {pair:?}"))?;
    if key.is_empty() {
        bail!("empty key in {pair:?}");
    }
    let value = serde_json::from_str::<Value>(raw).unwrap_or_else(|_| Value::String(raw.to_string()));
    Ok((key.to_string(), value))
}

fn cmd_set(image: &Path, pairs: &[String]) -> Result<()> {
    let data = load_container(image)?;
    let (start, size) = json_box_range(&data)?
        .with_context(|| format!("no \"json\" box in {} (use attach)", image.display()))?;
    let header = if data[start..start + 4] == 1u32.to_be_bytes() {
        16
    } else {
        8
    };
    let existing = &data[start + header..start + size];

    let mut obj: Map<String, Value> = match serde_json::from_slice(existing) {
        Ok(Value::Object(m)) => m,
        Ok(_) => bail!("{}: json box is not an object", image.display()),
        Err(e) => bail!("{}: json box is not valid JSON: {e}", image.display()),
    };

    for pair in pairs {
        let (k, v) = parse_assignment(pair)?;
        obj.insert(k, v);
    }

    let payload = serde_json::to_vec(&Value::Object(obj))?;
    let mut out = Vec::with_capacity(data.len() - size + 8 + payload.len());
    out.extend_from_slice(&data[..start]);
    out.extend_from_slice(&encode_box(JSON_BOX, &payload));
    out.extend_from_slice(&data[start + size..]);
    replace_file(image, &out)?;

    outln!("set {} key(s) in {}", pairs.len(), image.display());
    Ok(())
}

fn cmd_rm(image: &Path) -> Result<()> {
    let data = load_container(image)?;
    let (start, size) = json_box_range(&data)?
        .with_context(|| format!("no \"json\" box in {}", image.display()))?;

    let mut out = Vec::with_capacity(data.len() - size);
    out.extend_from_slice(&data[..start]);
    out.extend_from_slice(&data[start + size..]);
    replace_file(image, &out)?;

    outln!("removed \"json\" box from {}", image.display());
    Ok(())
}

// --- hand-rolled YAML emitter -------------------------------------------------

fn yaml_scalar(s: &str) -> String {
    let plain = !s.is_empty()
        && s.chars().all(|c| {
            c.is_ascii_alphanumeric() || matches!(c, ' ' | '_' | '-' | '.' | '/' | '+' | '@' | '(' | ')')
        })
        && !s.starts_with(' ')
        && !s.ends_with(' ')
        && s.parse::<f64>().is_err()
        && !matches!(
            s.to_ascii_lowercase().as_str(),
            "true" | "false" | "null" | "yes" | "no" | "on" | "off" | "~"
        );

    if plain {
        return s.to_string();
    }

    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn yaml_emit(v: &Value, indent: usize, out: &mut String) {
    let pad = " ".repeat(indent);
    match v {
        Value::Object(m) if !m.is_empty() => {
            for (k, val) in m {
                match val {
                    Value::Object(o) if !o.is_empty() => {
                        out.push_str(&format!("{pad}{}:", yaml_scalar(k)));
                        out.push('\n');
                        yaml_emit(val, indent + 2, out);
                    }
                    Value::Array(a) if !a.is_empty() => {
                        out.push_str(&format!("{pad}{}:", yaml_scalar(k)));
                        out.push('\n');
                        yaml_emit(val, indent + 2, out);
                    }
                    _ => {
                        out.push_str(&format!("{pad}{}: {}\n", yaml_scalar(k), yaml_inline(val)));
                    }
                }
            }
        }
        Value::Array(a) if !a.is_empty() => {
            for item in a {
                match item {
                    Value::Object(o) if !o.is_empty() => {
                        out.push_str(&format!("{pad}-\n"));
                        yaml_emit(item, indent + 2, out);
                    }
                    Value::Array(x) if !x.is_empty() => {
                        out.push_str(&format!("{pad}-\n"));
                        yaml_emit(item, indent + 2, out);
                    }
                    _ => out.push_str(&format!("{pad}- {}\n", yaml_inline(item))),
                }
            }
        }
        _ => out.push_str(&format!("{pad}{}\n", yaml_inline(v))),
    }
}

fn yaml_inline(v: &Value) -> String {
    match v {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n) => n.to_string(),
        Value::String(s) => yaml_scalar(s),
        Value::Array(a) if a.is_empty() => "[]".to_string(),
        Value::Object(m) if m.is_empty() => "{}".to_string(),
        _ => String::new(),
    }
}

// -----------------------------------------------------------------------------

fn default_jxl_files() -> Result<Vec<PathBuf>> {
    let entries = fs::read_dir(".").context("reading current directory")?;
    Ok(entries
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|e| e == "jxl"))
        .collect())
}

/// Emit one line of NDJSON: {"path": ..., "meta": ...}. Used when stdout
/// is not a terminal, so jxlmeta list stays pipeable into jq and friends.
fn emit_ndjson(path: &Path, v: &Value) -> Result<()> {
    let mut obj = Map::new();
    obj.insert("path".to_string(), Value::String(path.display().to_string()));
    obj.insert("meta".to_string(), v.clone());
    let mut line = serde_json::to_vec(&Value::Object(obj))?;
    line.push(b'\n');
    let _ = std::io::stdout().write_all(&line);
    Ok(())
}

fn cmd_list(files: Vec<PathBuf>) -> Result<()> {
    let files = if files.is_empty() {
        default_jxl_files()?
    } else {
        files
    };

    let tty = std::io::stdout().is_terminal();

    for path in &files {
        if !path.is_file() {
            continue;
        }
        let Ok(data) = fs::read(path) else { continue };
        if !is_container(&data) {
            continue;
        }
        let Ok(Some(payload)) = find_json_box(&data) else {
            continue;
        };

        match serde_json::from_slice::<Value>(&payload) {
            Ok(v) => {
                if tty {
                    outln!("# {}", path.display());
                    let mut s = String::new();
                    yaml_emit(&v, 0, &mut s);
                    let _ = std::io::stdout().write_all(s.as_bytes());
                    outln!();
                } else {
                    emit_ndjson(path, &v)?;
                }
            }
            Err(_) if tty => {
                outln!("# invalid JSON in {}", path.display());
                for line in String::from_utf8_lossy(&payload).lines() {
                    outln!("# {line}");
                }
                outln!();
            }
            Err(_) => {}
        }
    }
    Ok(())
}

fn cmd_lint() -> Result<()> {
    let re = Regex::new(r"^([0-9a-f]{64})\.jxl$").expect("regex");
    let entries = fs::read_dir(".").context("reading current directory")?;

    for entry in entries.filter_map(|e| e.ok()) {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let fname = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if let Some(caps) = re.captures(fname) {
            let expected = &caps[1];
            let data = match fs::read(&path) {
                Ok(d) => d,
                Err(_) => continue,
            };
            let actual = digest(&data);
            if expected != actual {
                outln!(
                    "# MISMATCH: {} (expected {expected}, actual {actual})",
                    path.display()
                );
                outln!("mv -v {} {actual}.jxl", path.display());
            }
        }
    }
    Ok(())
}

fn run() -> Result<()> {
    match Cli::parse().cmd {
        Cmd::Attach { image, meta } => cmd_attach(&image, &meta),
        Cmd::Get { image } => cmd_get(&image),
        Cmd::Set { image, pairs } => cmd_set(&image, &pairs),
        Cmd::Rm { image } => cmd_rm(&image),
        Cmd::List { files } => cmd_list(files),
        Cmd::Lint => cmd_lint(),
        Cmd::Completions { shell } => {
            let mut cmd = Cli::command();
            let name = cmd.get_name().to_string();
            clap_complete::generate(shell, &mut cmd, &name, &mut std::io::stdout());
            Ok(())
        }
    }
}

fn main() {
    if let Err(e) = run() {
        if let Some(ioe) = e.downcast_ref::<std::io::Error>() {
            if ioe.kind() == std::io::ErrorKind::BrokenPipe {
                std::process::exit(0);
            }
        }
        eprintln!("Error: {e:?}");
        std::process::exit(1);
    }
}
