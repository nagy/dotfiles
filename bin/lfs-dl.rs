#!/usr/bin/env rust-script
//! ```cargo
//! [dependencies]
//! clap = { version = "4", features = ["derive"] }
//! clap_complete = "4"
//! anyhow = "1"
//! glob = "0.3"
//! ```

use std::{
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::Result;
use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::Shell;

/// Report which git-lfs-tracked files are downloaded (content present in the
/// local LFS object store) in (bare) mirror repos.
#[derive(Parser)]
#[command(about)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// List tracked files whose content is in the local LFS store
    Present {
        /// Only consider tracked paths matching this glob
        #[arg(long, default_value = "*")]
        filter: String,
        /// Append the introducing commit and the LFS object path
        #[arg(long)]
        verbose: bool,
        /// Repos to check (default: current directory)
        repos: Vec<PathBuf>,
    },
    /// List tracked files whose content is NOT in the local LFS store
    Missing {
        #[arg(long, default_value = "*")]
        filter: String,
        /// Append the commit of the version you lack
        #[arg(long)]
        verbose: bool,
        repos: Vec<PathBuf>,
    },
    /// One summary line per repo: present/total at HEAD, store objects + size
    Status {
        #[arg(long, default_value = "*")]
        filter: String,
        repos: Vec<PathBuf>,
    },
    /// Raw LFS store inventory: object count and total size
    Store { repos: Vec<PathBuf> },
    /// Generate shell completions
    Completions {
        /// The shell to generate completions for
        #[arg(value_enum)]
        shell: Shell,
    },
}

fn main() -> Result<()> {
    match Cli::parse().cmd {
        Cmd::Present {
            filter,
            verbose,
            repos,
        } => {
            run_in_repos(&repos, |repo, many| {
                show_present(repo, &filter, verbose, many)
            });
        }
        Cmd::Missing {
            filter,
            verbose,
            repos,
        } => {
            run_in_repos(&repos, |repo, many| {
                show_missing(repo, &filter, verbose, many)
            });
        }
        Cmd::Status { filter, repos } => {
            run_in_repos(&repos, |repo, _| show_status(repo, &filter));
        }
        Cmd::Store { repos } => {
            run_in_repos(&repos, |repo, _| show_store(repo));
        }
        Cmd::Completions { shell } => {
            let mut cmd = Cli::command();
            let name = cmd.get_name().to_string();
            clap_complete::generate(shell, &mut cmd, &name, &mut std::io::stdout());
        }
    }
    Ok(())
}

// Validate each repo (defaulting to ".") and run `f` inside it.
fn run_in_repos(repos: &[PathBuf], f: impl Fn(&Path, bool)) {
    let repos: Vec<PathBuf> = if repos.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        repos.to_vec()
    };
    let many = repos.len() > 1;
    for repo in &repos {
        if !(repo.join("objects").is_dir() && repo.join("HEAD").is_file()) {
            eprintln!("lfs-dl: not a bare repo: {}", repo.display());
            continue;
        }
        f(repo, many);
    }
}

// NUL-separated list of tracked paths at HEAD (also handles paths containing
// newlines, which the shell's `mapfile -t` would have split).
fn tracked_files(repo: &Path) -> Vec<String> {
    let out = Command::new("git")
        .args(["ls-tree", "-r", "-z", "--name-only", "HEAD"])
        .current_dir(repo)
        .output();
    match out {
        Ok(o) if o.status.success() => o
            .stdout
            .split(|&b| b == 0)
            .filter_map(|b| std::str::from_utf8(b).ok())
            .filter(|s| !s.is_empty())
            .map(str::to_string)
            .collect(),
        _ => Vec::new(),
    }
}

// Bare-hex LFS oid of a tracked path at HEAD, or None if it is not an LFS
// pointer. Only the first 300 bytes are read: LFS pointers are ~130 bytes,
// anything bigger is a plain blob and can't be a pointer.
fn oid_at_head(repo: &Path, f: &str) -> Option<String> {
    let spec = format!("HEAD:{f}");
    let out = Command::new("git")
        .args(["show", &spec])
        .current_dir(repo)
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let head: Vec<u8> = out.stdout.into_iter().take(300).collect();
    for line in String::from_utf8_lossy(&head).lines() {
        if let Some(oid) = line.strip_prefix("oid sha256:") {
            let oid = oid.trim();
            if !oid.is_empty() {
                return Some(oid.to_string());
            }
        }
    }
    None
}

// Short hash of the most recent commit where the pointer for `f` became (or
// was) the version with oid `oid`; None if not found.
fn commit_for_oid(repo: &Path, f: &str, oid: &str) -> Option<String> {
    let pickaxe = format!("oid sha256:{oid}");
    let out = Command::new("git")
        .args(["log", "-1", "--format=%h", "-S", &pickaxe, "--", f])
        .current_dir(repo)
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}

fn lfs_obj_path(oid: &str) -> String {
    let a = oid.get(0..2).unwrap_or("");
    let b = oid.get(2..4).unwrap_or("");
    format!("lfs/objects/{a}/{b}/{oid}")
}

fn matches_filter(filter: &str, f: &str) -> bool {
    glob::Pattern::new(filter)
        .map(|p| p.matches(f))
        .unwrap_or(false)
}

// Number of entries at lfs/objects/<2>/<2>/<oid>.
fn count_store_objects(repo: &Path) -> usize {
    let mut count = 0;
    if let Ok(d1) = std::fs::read_dir(repo.join("lfs/objects")) {
        for e1 in d1.flatten() {
            if let Ok(d2) = std::fs::read_dir(e1.path()) {
                for e2 in d2.flatten() {
                    if let Ok(d3) = std::fs::read_dir(e2.path()) {
                        count += d3.flatten().count();
                    }
                }
            }
        }
    }
    count
}

fn store_size(repo: &Path) -> String {
    let out = Command::new("du")
        .args(["-sh", "lfs/objects"])
        .current_dir(repo)
        .output()
        .ok();
    match out {
        Some(o) if o.status.success() => String::from_utf8_lossy(&o.stdout)
            .split_whitespace()
            .next()
            .unwrap_or("0")
            .to_string(),
        _ => "0".to_string(),
    }
}

fn repo_basename(repo: &Path) -> String {
    if repo == Path::new(".") {
        std::env::current_dir()
            .ok()
            .and_then(|p| p.file_name().map(|s| s.to_string_lossy().into_owned()))
            .unwrap_or_else(|| ".".to_string())
    } else {
        repo.file_name()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| ".".to_string())
    }
}

fn print_file(repo: &Path, f: &str, oid: &str, obj: &str, verbose: bool, want_present: bool) {
    if verbose {
        if let Some(c) = commit_for_oid(repo, f, oid) {
            if want_present {
                println!("{f} ({c})\t{obj}");
            } else {
                println!("{f} ({c})");
            }
            return;
        }
        if want_present {
            println!("{f}\t{obj}");
            return;
        }
    }
    println!("{f}");
}

fn show_present(repo: &Path, filter: &str, verbose: bool, many: bool) {
    if many {
        println!("== {} ==", repo.display());
    }
    for f in tracked_files(repo) {
        if !matches_filter(filter, &f) {
            continue;
        }
        let Some(oid) = oid_at_head(repo, &f) else {
            continue;
        };
        let obj = lfs_obj_path(&oid);
        if repo.join(&obj).is_file() {
            print_file(repo, &f, &oid, &obj, verbose, true);
        }
    }
}

fn show_missing(repo: &Path, filter: &str, verbose: bool, many: bool) {
    if many {
        println!("== {} ==", repo.display());
    }
    for f in tracked_files(repo) {
        if !matches_filter(filter, &f) {
            continue;
        }
        let Some(oid) = oid_at_head(repo, &f) else {
            continue;
        };
        let obj = lfs_obj_path(&oid);
        if !repo.join(&obj).is_file() {
            print_file(repo, &f, &oid, &obj, verbose, false);
        }
    }
}

fn show_status(repo: &Path, filter: &str) {
    let mut present = 0usize;
    let mut total = 0usize;
    for f in tracked_files(repo) {
        if !matches_filter(filter, &f) {
            continue;
        }
        let Some(oid) = oid_at_head(repo, &f) else {
            continue;
        };
        total += 1;
        if repo.join(lfs_obj_path(&oid)).is_file() {
            present += 1;
        }
    }
    println!(
        "{}: {}/{} at HEAD, store: {} obj / {}",
        repo_basename(repo),
        present,
        total,
        count_store_objects(repo),
        store_size(repo),
    );
}

fn show_store(repo: &Path) {
    println!(
        "{}: {} objects / {}",
        repo_basename(repo),
        count_store_objects(repo),
        store_size(repo),
    );
}
