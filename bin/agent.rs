#!/usr/bin/env rust-script
//! ```cargo
//! [dependencies]
//! anyhow = "1"
//! ```

//! Sandboxed AI-agent runner. Mirrors the (previously shell) `agent`
//! launcher: build a bubblewrap namespace and exec the requested command
//! inside it.
//!
//! Invoked as: agent [args...]
//!   The trailing args are passed verbatim to the sandboxed command.

use anyhow::{bail, Result};
use std::{
    env,
    path::{Path, PathBuf},
    process::{self, Command},
};

/// Expand a leading `~` to `home`. Returns `input` unchanged when it does not
/// start with `~` (or when `home` is empty).
fn expand_tilde(input: &str, home: &str) -> String {
    match input.strip_prefix('~') {
        Some(_) if home.is_empty() => input.to_string(),
        Some(rest) if rest.starts_with('/') => format!("{home}{rest}"),
        Some(rest) if rest.is_empty() => home.to_string(),
        // `~user` forms are not supported.
        _ => input.to_string(),
    }
}

fn main() -> Result<()> {
    let home = match env::var("HOME") {
        Ok(h) if !h.is_empty() => h,
        _ => bail!("HOME is not set"),
    };
    let pi_dir = PathBuf::from(&home).join(".pi");

    let args: Vec<String> = env::args().skip(1).collect();

    // If the first argument starts with `--`, the leading args are bwrap
    // options (until a `--` separator, after which the inner command begins).
    // Otherwise the args are the command, as before.
    let (bwrap_args, command): (Vec<String>, Vec<String>) =
        if args.first().is_some_and(|a| a.starts_with("--")) {
            match args.iter().position(|a| a == "--") {
                Some(i) => (args[..i].to_vec(), args[i + 1..].to_vec()),
                None => (args.clone(), Vec::new()),
            }
        } else {
            (Vec::new(), args)
        };
    let command: Vec<String> = if command.is_empty() {
        vec![env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string())]
    } else {
        command
    };

    let mut b = Command::new("bwrap");

    // Bind mounts.
    b.args([
        "--dev",
        "/dev",
        "--proc",
        "/proc",
        "--ro-bind",
        "/run/current-system",
        "/run/current-system",
        "--ro-bind",
        "/etc/ssl/",
        "/etc/ssl/",
        "--ro-bind",
        "/etc/static/ssl/",
        "/etc/static/ssl/",
        "--ro-bind",
        "/etc/nix/nix.conf",
        "/etc/nix/nix.conf",
        "--ro-bind",
        "/etc/nix/registry.json",
        "/etc/nix/registry.json",
        "--ro-bind",
        "/nix",
        "/nix",
        "--ro-bind",
        "/bin/sh",
        "/bin/sh",
        "--ro-bind",
        "/usr/bin/env",
        "/usr/bin/env",
        "--tmpfs",
        "/tmp",
    ]);

    b.arg("--bind");
    b.arg(PathBuf::from(&home).join("agent"));
    b.arg(&home);

    if pi_dir.is_dir() {
        b.arg("--bind");
        b.arg(&pi_dir);
        b.arg(&pi_dir);
    }

    // Namespace / privilege setup.
    b.args([
        "--uid",
        "0",
        "--gid",
        "0",
        "--unshare-user",
        "--unshare-pid",
        "--unshare-uts",
        "--unshare-ipc",
        "--clearenv",
    ]);

    // Environment.
    b.args(["--setenv", "PATH"]);
    if let Ok(v) = env::var("PATH") {
        b.arg(&v);
    } else {
        b.arg("/usr/bin:/bin");
    }

    b.args(["--setenv", "TERM"]);
    match env::var("TERM") {
        Ok(v) if !v.is_empty() => {
            b.arg(v);
        }
        _ => {
            b.arg("dumb");
        }
    }

    b.args(["--setenv", "EDITOR", "nvim"]);
    b.args(["--setenv", "HOME", &home]);

    for var in ["XDG_RUNTIME_DIR", "NIX_PATH"] {
        if let Ok(v) = env::var(var) {
            if !v.is_empty() {
                b.args(["--setenv", var, &v]);
            }
        }
    }

    // Copy additional env vars from AGENT_ENVS (comma-separated list of
    // variable names, or `name=value` literals) into the sandbox. Commas
    // (and not colons) are used so that PATH-style values are not split.
    //   `AGENT_ENVS=FOO,BAR`      copies host FOO/BAR
    //   `AGENT_ENVS=FOO=1,BAR`    sets FOO to literal "1", copies BAR
    if let Ok(entries) = env::var("AGENT_ENVS") {
        for entry in entries.split(',') {
            if entry.is_empty() {
                continue;
            }
            // Split on the FIRST `=`; `name=value` literals override lookups.
            if let Some((name, value)) = entry.split_once('=') {
                if !name.is_empty() && !value.is_empty() {
                    b.args(["--setenv", name, &expand_tilde(value, &home)]);
                }
            } else if let Ok(v) = env::var(entry) {
                if !v.is_empty() {
                    b.args(["--setenv", entry, &v]);
                }
            }
        }
    }

    // Bind exact directories from AGENT_BINDS / AGENT_ROBINDS
    // (comma-separated, `~` expanded). Each entry is `src` (mapping to its own
    // path) or `src=dest`. A missing source is an error.
    for (flag, var) in [("--bind", "AGENT_BINDS"), ("--ro-bind", "AGENT_ROBINDS")] {
        if let Ok(entries) = env::var(var) {
            for entry in entries.split(',') {
                if entry.is_empty() {
                    continue;
                }
                let (src, dest) = match entry.split_once('=') {
                    Some((s, d)) if !s.is_empty() && !d.is_empty() => {
                        (expand_tilde(s, &home), expand_tilde(d, &home))
                    }
                    _ => {
                        let p = expand_tilde(entry, &home);
                        (p.clone(), p)
                    }
                };
                if !Path::new(&src).exists() {
                    bail!("agent: {var} entry not found: {src}");
                }
                b.arg(flag);
                b.arg(&src);
                b.arg(&dest);
            }
        }
    }

    // Forward user-supplied bwrap options, then the inner command.
    b.args(&bwrap_args);
    b.args(&command);

    let status = b.status()?;
    match status.code() {
        Some(code) => process::exit(code),
        None => {
            use std::os::unix::process::ExitStatusExt;
            process::exit(128 + status.signal().unwrap_or(1));
        }
    }
}
