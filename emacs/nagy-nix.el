;;; nagy-nix.el --- My nix config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(require 'general)

;;;###autoload (autoload 'nixfmt-buffer "nagy-nix" nil t)
;;;###autoload (autoload 'nixfmt-region "nagy-nix" nil t)
;;;###autoload (autoload 'nixfmt-on-save-mode "nagy-nix" nil t)
(reformatter-define nixfmt
  :program "nixfmt"
  :args (list input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file-in-current-directory)
  :group 'nix)

;; NIX-EMACS-PACKAGE: nix-mode
(use-package nix-mode
  :custom
  (nix-repl-executable-args
   '("repl" "--expr" "with import <nixpkgs> {}; builtins // lib // pkgs // pkgs.nur.repos.nagy.lib"))
  :defer t
  ;; :hook
  ;; (nix-mode-hook . eglot-ensure)
  :hook
  (nix-mode-hook . nixfmt-on-save-mode)
  :general
  (:states 'normal :keymaps 'nix-mode-map
           "⊢" #'nixfmt-buffer)
  ;; :bind
  ;; (:map nix-mode-map
  ;;       ("C-⊢" . nixfmt-buffer))
  ;; :general
  ;; (:states 'normal :keymaps 'nix-mode-map
  ;;          "⊢" #'nixfmt-buffer)
  :bind
  ("H-M-n" . nix-mode))


;; NIX-EMACS-PACKAGE: nix-ts-mode
(use-package nix-ts-mode
  :defer t)
;; :config
;; (setq treesit-font-lock-level 4)
;; (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))


(define-auto-insert
  '("\\.nix\\'" . "Nix skeleton")
  '("Short description: "
    "{" \n
    "pkgs ? import <nixpkgs> { }," \n
    "# pkgs ? import (builtins.fetchTarball \"https://github.com/NixOS/nixpkgs/archive/pull/123456/head.tar.gz\") { }," \n
    "lib ? pkgs.lib," \n
    "stdenv ? pkgs.stdenv," \n
    "# fetchFromGitHub ? pkgs.fetchFromGitHub," \n
    "# writableTmpDirAsHomeHook ? pkgs.writableTmpDirAsHomeHook," \n
    "}:" \n
    \n
    "stdenv.mkDerivation (finalAttrs: {" \n
    "pname = \"\";" \n
    "version = \"\";" \n
    \n
    "src = fetchFromGitHub {" \n
    "owner = \"nagy\";" \n
    "repo = \"\";" \n
    "tag = \"\";" \n
    "# rev = \"\";" \n
    "# fetchSubmodules = true;" \n
    "hash = \"\";" \n
    "};" \n
    \n
    "# nativeBuildInputs = [ ];" \n
    "# buildInputs = [ ];" \n
    \n
    "# buildPhase = ''" \n
    "# runHook preBuild" \n
    "# runHook postBuild" \n
    "# '';" \n
    "# installPhase = ''" \n
    "# runHook preInstall" \n
    "# runHook postInstall" \n
    "# '';" \n

    "# preferLocalBuild = true;" \n
    "# allowSubstitutes = true;" \n

    "# meta = {" \n
    "# };" \n
    "})" \n
    \n))


(defun find-file-directory-nix ()
  (interactive)
  (cond
   ((file-exists-p "default.nix")
    (find-file "default.nix"))
   ((file-exists-p "flake.nix")
    (find-file "flake.nix"))
   (t (user-error "No Nix suitable file found"))))
(require 'dired)
(keymap-set dired-mode-map "H-M-n" #'find-file-directory-nix)
;; (with-eval-after-load 'tired-mode
;;   (keymap-set tired-mode-map "H-M-n" #'find-file-directory-nix))

(use-package nix-prettify-mode
  :diminish nix-prettify-mode
  :custom
  (nix-prettify-char "┃▒"))
;; :hook
;; (dired-mode-hook . nix-prettify-mode)
;; (nix-repl-mode-hook . nix-prettify-mode)


;; TODO has an lsp nls
;; NIX-EMACS-PACKAGE: nickel-mode
(use-package nickel-mode
  :preface
  (reformatter-define nickel-format
    :group 'nickel
    :program "nickel"
    :args '("format"))
  :defer t
  :hook
  (nickel-mode-hook . nickel-format-on-save-mode))

;; NIX-EMACS-PACKAGE: nixos
(use-package nixos
  :defer t
  :init
  (nagy-same-window
   (rx bos
       (or "*nixos-package "
           "*nixos-option "
           "*nixos-flake ")))
  (nagy-same-window (rx bos "*Embark Export: nixos-package"))
  (nagy-same-window (rx bos "*Embark Export: nixos-option"))
  :general
  (:states 'normal
           "○ ○" #'nixos-flake) ;; this does not work. maybe gets overwritten?
  (:states 'normal :keymaps 'nixos-browse-mode-map
           [remap evil-append] #'magit-status
           [remap evil-replace] #'revert-buffer
           ;; does this break region selection copying?
           [remap evil-yank] #'nixos-browse-copy-store-path)
  (:states 'normal :keymaps 'nixos-browse-packages-mode-map
           [remap evil-ret] #'nixos-browse-packages-visit)
  (:states 'normal :keymaps 'nixos-browse-options-mode-map
           [remap evil-ret] #'nixos-browse-options-visit)
  (:states 'normal :keymaps 'nixos-browse-flakes-mode-map
           [remap evil-ret] #'nixos-browse-flakes-visit))

;; (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))

;;; Nix store

(declare-function nix--process-string "nix" t)
(declare-function nix-instantiate "nix-instantiate" t t)
(declare-function evil-global-set-key "evil")
(declare-function dired-get-marked-files "dired")

(defconst nagy-nix-store-name-allowed-chars
  "a-zA-Z0-9+._?=-"
  "Characters that Nix permits in a store path name.
Nix rejects every other character.  See the `nix store add'
manual page.")

(defun nagy-nix-store-name (file)
  "Return a store path name for FILE that Nix accepts.
Nix takes the store path name from the base name of FILE.  This
function replaces each character that Nix rejects with a hyphen.
It keeps the original base name when Nix accepts it."
  (let* ((base (file-name-nondirectory (directory-file-name
                                        (expand-file-name file))))
         (safe (replace-regexp-in-string
                (concat "[^" nagy-nix-store-name-allowed-chars "]")
                "-" base)))
    (if (string= base safe)
        base
      (let ((trimmed (string-trim (replace-regexp-in-string "-+" "-" safe)
                                  "-" "-")))
        (if (string-empty-p trimmed) "file" trimmed)))))

(defun nagy-nix-store-add (path &optional name)
  "Copy PATH into the Nix store and return the new store path.
PATH is a regular file or a directory.  Nix takes the store path
name from the base name of PATH.  This function replaces each
character that Nix rejects with a hyphen.  NAME overrides the
store path name."
  (require 'nix)
  (let* ((path (expand-file-name path))
         (base (file-name-nondirectory (directory-file-name path)))
         (safe (nagy-nix-store-name path))
         (name (or name (unless (string= base safe) safe))))
    (apply #'nix--process-string
           (append '("store" "add")
                   (when name (list "--name" name))
                   (list path)))))

(defun nagy-nix-store-add-file-read-args ()
  "Read arguments for the Nix store add commands.
Return a list of the file name and the store path name.  The
store path name is nil unless the user gives a prefix argument."
  (let* ((file (read-file-name "Add to Nix store: " nil nil t))
         (name (when current-prefix-arg
                 (read-string "Store path name: "
                              (nagy-nix-store-name file)))))
    (list file name)))

(defun nagy-nix-show-store-path (store-path)
  "Display STORE-PATH.
Save STORE-PATH to the kill ring when package nix-store gives no
viewer."
  (require 'nix-store nil t)
  (if (fboundp 'nix-store-show-path)
      (nix-store-show-path store-path)
    (kill-new store-path)
    (message "Added %s" store-path)))

(defun nix-store-add-file (file &optional name)
  "Add FILE to the Nix store and show the new store path.
With prefix argument, read NAME and use it as the store path name.

Return the new store path."
  (interactive (nagy-nix-store-add-file-read-args))
  (let ((store-path (nagy-nix-store-add file name)))
    (when (called-interactively-p 'interactive)
      (nagy-nix-show-store-path store-path))
    store-path))

(defun nix-store-yank-file (file &optional name)
  "Add FILE to the Nix store and save the store path to the kill ring.
With prefix argument, read NAME and use it as the store path name.

Return the new store path."
  (interactive (nagy-nix-store-add-file-read-args))
  (let ((store-path (nagy-nix-store-add file name)))
    (kill-new store-path)
    (message "Killed: %s" store-path)
    store-path))

(defcustom nagy-nix-unpack-attribute "nur.repos.nagy.lib.unpackFile"
  "Nix attribute that unpacks one archive file.
The attribute receives the archive file in the `src' argument."
  :group 'nix
  :type 'string)

(defun nagy-nix-unpack-file-at-point ()
  "Return the file name for `nix-unpack'.
Return the marked file in Dired, else the file name at point."
  (or (when (derived-mode-p 'dired-mode)
        (car (dired-get-marked-files)))
      (thing-at-point 'filename t)))

(defun nagy-nix-build-derivation (drv)
  "Build DRV and return the first output store path."
  (require 'nix)
  (let* ((lines (process-lines nix-executable "build" "--no-link"
                               "--print-out-paths" (concat drv "^*")))
         (prefix (concat nix-store-dir "/"))
         (outputs (seq-filter (lambda (line) (string-prefix-p prefix line))
                              lines)))
    (or (car outputs)
        (user-error "No store path in the build output: %s" lines))))

(defun nix-unpack (file)
  "Unpack FILE into the Nix store and return the output path.
FILE is an archive file such as a tarball or a zip file.  The
store path of FILE goes to `nagy-nix-unpack-attribute'.

When called interactively, visit the output path."
  (interactive
   (let ((candidate (nagy-nix-unpack-file-at-point)))
     (list (if (and candidate (file-exists-p candidate))
               candidate
             (read-file-name "Unpack file: " nil nil t)))))
  (require 'nix-instantiate)
  (let* ((src (nagy-nix-store-add file))
         (drv (nix-instantiate "<nixpkgs>" nagy-nix-unpack-attribute nil
                               `((src . ,src))))
         (out (nagy-nix-build-derivation drv)))
    (when (called-interactively-p 'interactive)
      (find-file out))
    out))

(with-eval-after-load 'evil
  (evil-global-set-key 'motion (kbd "○ a") #'nix-store-add-file)
  (evil-global-set-key 'motion (kbd "○ y") #'nix-store-yank-file)
  (evil-global-set-key 'motion (kbd "○ u") #'nix-unpack))

(provide 'nagy-nix)
;;; nagy-nix.el ends here
