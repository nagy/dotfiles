;;; nagy-tired.el --- Tired config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: tired
(use-package tired
  ;; :defer t
  :init
  (nagy-same-window "^Tired: ")
  (nagy-same-window "^\\*Tired Export ")
  (tired-find-file-mode 1)
  :custom
  (tired-directory-size t)
  (tired-shorthand-rules '(("\\`/nix/store/" . "○/")))
  ;; Anchored, case-insensitive names.  A hidden directory takes its
  ;; inserted children with it.
  (tired-hidden-regexps
   (list (rx bos (or "flake.lock" "Cargo.lock" "go.sum"
                     ".git" ".gitignore" ".dockerignore" ".gitleaksignore" ".gitattributes"
                     "LICENSE" "COPYING")
             eos)))
  ;; Dired takes these colors from `nagy-dired-font-lock-keywords'.
  ;; A tired rule with a submatch index paints that submatch only, so
  ;; Tired colors the extension alone and leaves the base name plain,
  ;; the way a Dired Font Lock rule does.  Partial rules run in list
  ;; order and a later rule paints over an earlier one, so the plain
  ;; extension rules come first and the narrow rules follow.  A rule
  ;; without an index paints the whole name, and a directory keeps its
  ;; own face.  The `invisible' rules stay in Dired.
  (tired-name-faces
   `(;; Underline.  Dired sets these with an `ov-set' overlay that
     ;; holds the underline alone, so each rule adds no color.  The
     ;; rule paints the whole name, and the extension rule paints
     ;; the color of its own span, so the name mixes both faces.
     ;; The third element is a face list, not an index, so each
     ;; rule stays a whole-name rule.
     (,(rx (or "kustomization.yaml") eos) (:underline t))
     (,(rx (or "book.journal") eos) (:underline t))
     (,(rx (or "README.org" "README.md") eos) (:underline t))
     (,(rx (or "default.nix" "flake.nix") eos) (:underline t))
     (,(rx (or "go.mod" ) eos) (:underline t))
     ;; Nix
     (,(rx (group "." (or "nix") eos)) nagy-intense-green 1)
     ;; Data
     (,(rx (group "." (or "json" "yaml" "yml" "jsonl" "toml" "xml" "csv" "ron"
                          "feather" "parquet"))
           (? "." (or "zst" "br")) eos)
      nagy-intense-red 1)
     ;; Media, photos
     (,(rx (group "." (or "png" "jpg" "jpeg" "webp" "jxl" "svg" "avif")
                  eos))
      nagy-intense-cyan 1)
     ;; Code
     (,(rx (group "." (or "rs" "js" "fs" "c" "cc" "cpp" "h" "zig" "go"
                          "ts" "py" "jq" "lisp" "asd" "scm" "hy" "lua"
                          "fnl" "wat" "c3" "css" "svelte" "typ" "pk"
                          "tf"))
           eos)
      nagy-intense-blue 1)
     ;; Code, Emacs Lisp
     (,(rx (group "." (or "el") eos)) nagy-intense-magenta 1)
     ;; Code, executable
     (,(rx (group "." (or "wasm" "cwasm") eos)) ansi-color-inverse 1)
     ;; Documentation
     (,(rx (group "." (or "md" "rst" "org" "txt" "pdf" "html" "gmi"
                          "mmd" "journal"))
           eos)
      nagy-intense-yellow 1)
     ;; Media, videos, audio
     (,(rx (group "." (or "mp4" "webm" "gif" "opus" "ogg" "mp3" "wav"
                          "avi" "mkv" "m4a" "flac"))
           eos)
      nagy-subtle-cyan 1)
     ;; Archives
     (,(rx (group "." (or "tar" "zip" "gitbundle" "sqfs") eos))
      eshell-ls-archive 1)
     ;; Compression
     (,(rx (group "." (or "zst" "gz" "bz2" "br" "xz") eos))
      (:background "grey" :foreground "black")
      1)
     ;; Irrelevant
     (,(rx (group (or ".gitignore" ".gitattributes" ".elpaignore"
                      ".dockerignore"
                      "LICENSE"
                      "flake.lock" "poetry.lock" "Cargo.lock")
                  eos))
      parenthesis 1)
     ;; Data, minor.  These paint the first extension only, so the
     ;; compression suffix keeps its own color.
     (,(rx (group "." (or "tokei" "ctags" "restic" "rclone" "ncdu" "jc"
                          "journal" "info" "docker" "ytdlp" "yggdrasil"
                          "busctl" "gh" "glab" "ss"))
           "." (or "json" "yaml" "yml" "jsonl" "toml" "xml" "csv" "ron"
                   "feather" "parquet")
           (? "." (or "zst" "br")) eos)
      nagy-subtle-red 1)
     ;; Code, minor.  They follow the plain extension rules they
     ;; overlap, because a later rule wins.
     (,(rx (group "." (or "jsonrpc")) "." (or "py") eos)
      nagy-subtle-blue 1)
     (,(rx (group "." (or "svelte")) "." (or "js" "ts") eos)
      nagy-subtle-blue 1)
     (,(rx (group (or "go.mod" "go.sum") eos)) nagy-subtle-blue 1)
     ;; Documentation, minor
     (,(rx (group "." (or "machine" "rsync" "lsremote" "cv"))
           "." (or "txt" "org" "md") eos)
      nagy-subtle-yellow 1)
     ;; Media, videos, minor
     (,(rx (group "." (or "thumbs"))
           "." (or "mp4" "webm" "gif" "opus" "ogg" "mp3" "wav" "avi"
                   "mkv" "m4a" "flac")
           eos)
      nagy-nuanced-cyan 1)))
  :general
  (:states 'normal :keymaps 'tired-mode-map
           ;; "o" #'dired-find-file-other-window
           "a" #'magit-status
           "ö" #'tired-browse-url-of-file
           "H-d" #'tired-delete
           "H-u" #'tired-undo
           "H-U" #'tired-redo)
  :bind
  (:map tired-name-edit-mode-map
        ([remap save-kill-buffer] . tired-commit-edits)
        ([remap kill-this-buffer] . tired-abort-edits)))

(provide 'nagy-tired)
;;; nagy-tired.el ends here
