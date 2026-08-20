#!/usr/bin/env bash
# lfs-dl.sh — report which git-lfs-tracked files are already downloaded
# (content present in the local git-lfs object store) in mirror repos.
set -euo pipefail

filter='*'
verbose=0
repos=()

usage() {
    cat <<EOF
Usage: lfs-dl.sh present [--filter GLOB] [--verbose] [REPO...]
       lfs-dl.sh missing [--filter GLOB] [--verbose] [REPO...]
       lfs-dl.sh status  [--filter GLOB] [REPO...]
       lfs-dl.sh store [REPO...]
       lfs-dl.sh help

Subcommands:
  present  List tracked files whose content is in the local LFS store
  missing  List tracked files whose content is NOT in the local LFS store
  status   One summary line per repo: present/total at HEAD, store objects + size
  store    Raw LFS store inventory: object count and total size
  help     Show this message

Options:
  --filter GLOB   Only consider tracked paths matching the shell glob
                  (default: * — all git-lfs-tracked files, any type)
  --verbose       present: append the introducing commit and the LFS object
                  path (cp source), e.g. "Q6_K_M.gguf (a1b2c3d)\tlfs/objects/49/33/…"
                  missing: append the commit of the version you lack

REPO: path to a (bare) mirror repo; defaults to the current directory.
      Multiple paths or globs work, e.g.:
      lfs-dl.sh status ~/mirrors/*/*.git

Notes:
  - Only git-lfs-tracked files are checked (LFS pointer content in HEAD);
    plain git blobs are always present and skipped.
  - Checks are HEAD-scoped: files present only in history are counted in
    'store' but not listed as 'present'.
  - Bare repos have no worktree; "downloaded" == object in lfs/objects.
EOF
}

# Parse "--filter GLOB", "--verbose" and repo args into the globals
# $filter, $verbose and $repos.
# $1: 1 if --filter is accepted, else 0. $2: 1 if --verbose is accepted.
parse_args() {
    local allow_filter="$1" allow_verbose="$2"; shift 2
    filter='*'
    verbose=0
    repos=()
    while (( $# > 0 )); do
        case "$1" in
            --filter)
                if (( allow_filter != 1 )); then
                    echo "lfs-dl.sh: --filter is not valid for this subcommand" >&2
                    exit 1
                fi
                if (( $# < 2 )); then
                    echo "lfs-dl.sh: --filter requires a GLOB argument" >&2
                    exit 1
                fi
                filter="$2"
                shift 2
                ;;
            --verbose)
                if (( allow_verbose != 1 )); then
                    echo "lfs-dl.sh: --verbose is not valid for this subcommand" >&2
                    exit 1
                fi
                verbose=1
                shift
                ;;
            *)
                repos+=("$1")
                shift
                ;;
        esac
    done
}

# Print the bare-hex LFS oid of a tracked path at HEAD (empty if the file
# is not an LFS pointer). Only the first 300 bytes are read: LFS pointers
# are ~130 bytes, anything bigger is a plain blob and can't be a pointer.
oid_at_head() {
    local f="$1"
    git show "HEAD:$f" 2>/dev/null | head -c 300 | sed -n 's/^oid sha256://p' || true
}

# Print the short hash of the most recent commit where the pointer for $f
# became (or was) the version with oid $oid; empty if not found.
commit_for_oid() {
    local f="$1" oid="$2"
    git log -1 --format=%h -S "oid sha256:$oid" -- "$f" 2>/dev/null || true
}

# Print one listed file. Under --verbose, present files get the introducing
# commit and the LFS object path (cp source); missing files get just the
# commit of the version you lack.
print_file() {
    local f="$1" oid="$2" obj="$3" want="$4" c=""
    if (( verbose == 1 )); then
        c=$(commit_for_oid "$f" "$oid")
        if [[ "$want" == present ]]; then
            if [[ -n "$c" ]]; then
                printf '%s (%s)\t%s\n' "$f" "$c" "$obj"
            else
                printf '%s\t%s\n' "$f" "$obj"
            fi
            return
        fi
        if [[ -n "$c" ]]; then
            printf '%s (%s)\n' "$f" "$c"
            return
        fi
    fi
    echo "$f"
}

# List tracked paths matching $filter, filtering on store presence.
# $1: "missing" → print only files NOT in the store; anything else → present.
list_lfs() {
    local want="$1"
    local f oid obj
    local files=()
    mapfile -t files < <(git ls-tree -r --name-only HEAD)
    for f in "${files[@]}"; do
        [[ "$f" == $filter ]] || continue
        oid=$(oid_at_head "$f")
        [[ -n "$oid" ]] || continue
        obj="lfs/objects/${oid:0:2}/${oid:2:2}/$oid"
        if [[ "$want" == missing ]]; then
            if [[ ! -f "$obj" ]]; then
                print_file "$f" "$oid" "$obj" "$want"
            fi
        else
            if [[ -f "$obj" ]]; then
                print_file "$f" "$oid" "$obj" "$want"
            fi
        fi
    done
}

# Run "$1" (a show_* function) inside each repo given in "${@:3}";
# default repo is the current directory. Passes "$filter", "$repo" and
# "$many" (1 if more than one repo, else 0) to the function.
run_in_repos() {
    local fn="$1" filter="$2"; shift 2
    local r many=0
    if (( ${#repos[@]} == 0 )); then
        repos=(.)
    fi
    (( ${#repos[@]} > 1 )) && many=1
    for r in "${repos[@]}"; do
        if [[ ! -d "$r/objects" || ! -f "$r/HEAD" ]]; then
            echo "lfs-dl.sh: not a bare repo: $r" >&2
            continue
        fi
        ( cd "$r" && "$fn" "$filter" "$r" "$many" )
    done
}

show_present() {
    local filter="$1" r="$2" many="$3"
    if [[ "$many" == 1 ]]; then
        echo "== $r =="
    fi
    list_lfs present "$filter"
}

show_missing() {
    local filter="$1" r="$2" many="$3"
    if [[ "$many" == 1 ]]; then
        echo "== $r =="
    fi
    list_lfs missing "$filter"
}

show_status() {
    local filter="$1" r="$2" many="$3"
    local f oid obj
    local files=() objs=()
    local present=0 total=0 size
    mapfile -t files < <(git ls-tree -r --name-only HEAD)
    for f in "${files[@]}"; do
        [[ "$f" == $filter ]] || continue
        oid=$(oid_at_head "$f")
        [[ -n "$oid" ]] || continue
        total=$((total+1))
        obj="lfs/objects/${oid:0:2}/${oid:2:2}/$oid"
        if [[ -f "$obj" ]]; then
            present=$((present+1))
        fi
    done
    shopt -s nullglob
    objs=( lfs/objects/*/*/* )
    size=$(du -sh lfs/objects 2>/dev/null | cut -f1) || true
    if [[ -z "$size" ]]; then
        size=0
    fi
    printf '%s: %d/%d at HEAD, store: %d obj / %s\n' \
        "$(basename "$PWD")" "$present" "$total" "${#objs[@]}" "$size"
}

show_store() {
    local filter="$1" r="$2" many="$3"
    local objs=() size
    shopt -s nullglob
    objs=( lfs/objects/*/*/* )
    size=$(du -sh lfs/objects 2>/dev/null | cut -f1) || true
    if [[ -z "$size" ]]; then
        size=0
    fi
    printf '%s: %d objects / %s\n' "$(basename "$PWD")" "${#objs[@]}" "$size"
}

cmd_present() { parse_args 1 1 "$@"; run_in_repos show_present "$filter" "${repos[@]}"; }
cmd_missing() { parse_args 1 1 "$@"; run_in_repos show_missing "$filter" "${repos[@]}"; }
cmd_status()  { parse_args 1 0 "$@"; run_in_repos show_status  "$filter" "${repos[@]}"; }
cmd_store()   { parse_args 0 0 "$@"; run_in_repos show_store   "$filter" "${repos[@]}"; }

case "${1:-help}" in
    present) shift; cmd_present "$@" ;;
    missing) shift; cmd_missing "$@" ;;
    status)  shift; cmd_status "$@" ;;
    store)   shift; cmd_store "$@" ;;
    help|--help|-h) usage; exit 0 ;;
    *) echo "lfs-dl.sh: unknown subcommand: $1" >&2; usage >&2; exit 1 ;;
esac
