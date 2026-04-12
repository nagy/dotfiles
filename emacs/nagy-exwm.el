;;; nagy-exwm.el --- config emacs packages -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") anaphora evil-escape nagy-emacs nagy-url nagy-gc)

;; NIX-EMACS-PACKAGE: modus-themes
(require 'modus-themes)

;; NIX-EMACS-PACKAGE: dash
(require 'dash)
;; NIX-EMACS-PACKAGE: evil
(require 'evil)
(eval-when-compile
  (require 'dired))

;; NIX-EMACS-PACKAGE: nagy-dired
(require 'nagy-dired)
(require 'key-chord)

(declare-function ibuffer-filter-by-used-mode "ibuffer")

(defun my-screenshot ()
  (interactive)
  (start-process-shell-command
   "screenshot"
   (get-buffer-create " *screenshot*")
   "scrot --select - | xclip -verbose -selection clipboard -t image/png")
  )
(keymap-global-set "s-<f5>" #'my-screenshot)

;; TODO prevent rollover
(defun nagy-move-tab-right (arg)
  (interactive "P")
  (tab-bar-move-tab (or arg 1)))
(defun nagy-move-tab-left (arg)
  (interactive "P")
  (tab-bar-move-tab (* -1 (or arg 1))))

(defun update-current-frame-fontset ()
  (interactive)
  ;; (set-fontset-font t 'unicode (font-spec :family "Noto Sans Symbols") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Noto Sans Symbols2") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Noto Sans Math") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Noto Sans Gothic") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "NotoMusic") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Iosevka Comfy") nil 'prepend)
  (set-fontset-font t ?⨎ (font-spec :family "DejaVu Sans") nil 'prepend)
  (set-fontset-font t ?⨏ (font-spec :family "DejaVu Sans") nil 'prepend)
  (set-fontset-font t ?⌯ (font-spec :family "Unifont"))
  (set-fontset-font t 'emoji (font-spec :family "Noto Emoji"))
  (set-fontset-font t ?⚶ (font-spec :family "DejaVu Sans"))
  (set-fontset-font t ?☂ (font-spec :family "Noto Emoji"))
  (set-fontset-font t ?⌬ (font-spec :family "DejaVu Sans"))
  (set-fontset-font t ?⦵ (font-spec :family "FreeSerif"))
  (set-fontset-font t ?☇ (font-spec :family "DejaVu Sans"))
  (set-fontset-font t ?⌥ (font-spec :family "DejaVu Sans"))
  (set-fontset-font t ?🗑 (font-spec :family "Noto Emoji"))
  ;; (set-fontset-font t ?⎇ (font-spec :family "Noto Emoji"))
  ;; (set-fontset-font t ?🔰 (font-spec :family "Noto Emoji"))
  ;; (set-fontset-font t ?❖ (font-spec :family "Noto Emoji"))
  ;; 🔷
  (set-fontset-font t ?⩎ (font-spec :family "Unifont"))
  (set-fontset-font t ?⩏ (font-spec :family "Unifont"))
  ;; These are "symbols"
  (set-fontset-font t ?🎛 (font-spec :family "Noto Emoji"))
  (set-fontset-font t ?❄ (font-spec :family "Noto Emoji"))
  (set-fontset-font t ?⛓ (font-spec :family "Noto Emoji"))
  (set-fontset-font t ?〒 (font-spec :family "Noto Emoji"))
  ;; (set-fontset-font t ?〜 (font-spec :family "Noto Emoji"))
  ;; force iosevka comfy
  (dolist (it '(?※ ?┋ ?○ ?⟻ ?✦ ?∨ ?≢ ?↡ ?⇣ ?⊢ ?⍳ ?⋮ ?⁝ ?♁ ?∴ ?⌿ ?◉
                   ?∅ ?∧ ?↻ ?⒈ ?⒉ ?⇘ ?⟼ ?☐ ?⋯ ?◇ ?□ ?∃ ?⍺ ?⬖ ?⬗
                   ?♪ ?⚇ ?∀ ?⌀ ?∄ ?✄ ?☉ ?∡ ?☿ ?ⓐ ?● ?♄ ?↞ ?↠ ?☆ ?◫
                   ?┄ ?♯ ?↧ ?⅞ ?⊻ ?⊽ ?⅜ ?⅛ ?∿ ?π ?〉 ?〈
                   ?┃ ?⍜ ?⋋ ?⋌ ?⏚ ?⎎ ?▱ ?◺ ?◸ ?▰ ?◿ ?◹
                   ?☈ ?↯ ?⇊ ?⇉ ?⇇ ?⇈ ?‼ ?↜ ?↝ ?⋱ ?⚆ ?∵ ?⏢ ?▫ ?⍼
                   ?⚀ ?⚁ ?⚂ ?⚃ ?⚄ ?⚅ ?⎵ ?⋔ ?⎍ ?⌂ ?δ))
  (set-fontset-font t it (font-spec :family "Iosevka Comfy")))
(dolist (it '(?▮ ?⧻))
  (set-fontset-font t it (font-spec :family "IosevkaTerm Nerd Font")))
)

(defvar brightness--value 0)

;; make (setf)-able
;; maybe: https://github.com/mschuldt/backlight.el ?
(defun brightness-up ()
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-incf brightness--value 5)
    (setq brightness--value (min 100 brightness--value))
    (dolist (backlight-driver-name (directory-files "/sys/class/backlight/" nil (rx (any "a-z"))))
      (call-process "brightnessctl" nil nil nil
                    (format "--device=%s" backlight-driver-name) "set" (format "%d%%" brightness--value)))
    (message "Brightness set to %d" brightness--value)))
(keymap-global-set "<XF86MonBrightnessUp>" #'brightness-up)

(defun brightness-down ()
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-decf brightness--value 5)
    (setq brightness--value (max 0 brightness--value))
    (dolist (backlight-driver-name (directory-files "/sys/class/backlight/" nil (rx (any "a-z"))))
      (call-process "brightnessctl" nil nil nil
                    (format "--device=%s" backlight-driver-name) "set" (format "%d%%" brightness--value)))
    (message "Brightness set to %d" brightness--value)))
(keymap-global-set "<XF86MonBrightnessDown>" #'brightness-down)

;; NIX-EMACS-PACKAGE: exwm
(use-package exwm
  :if (display-graphic-p)
  :commands (exwm-randr-refresh exwm-workspace-rename-buffer)
  :preface
  (defun tab-last ()
    (interactive)
    (tab-bar-switch-to-last-tab))
  (defun tab-first ()
    (interactive)
    (tab-bar-select-tab 1))
  (defun ibuffer-exwm ()
    (interactive)
    (if (get-buffer "*Ibuffer*")
        (progn
          (switch-to-buffer "*Ibuffer*")
          (revert-buffer))
      (ibuffer)
      (ibuffer-filter-by-used-mode 'exwm-mode)))
  (defun nagy-fix-frame ()
    "Frames directly started in exwm have some missing keys like s-💤.
aka xcompose is not properly initialized in the first frame."
    (interactive)
    (let ((it (car (frame-list))))
      (make-frame)
      (delete-frame it)
      (make-frame)
      (global-hl-line-mode -1)
      (call-interactively #'modus-themes-toggle)
      (call-interactively #'modus-themes-toggle)
      (global-hl-line-mode -1)
      (when (fboundp 'evil-escape-mode)
        (evil-escape-mode -1))
      (exwm-randr-refresh)
      (evil-mode)
      (GC-DISABLE)
      (setq menu-updating-frame nil) ; without this, kill-current-buffer is broken
      (setq-default lexical-binding t)
      (update-current-frame-fontset)))
  (defun nagy-exwm-rename-buffer ()
    (exwm-workspace-rename-buffer
     (--> (concat (pcase exwm-class-name
                    ("Alacritty" "")
                    ("firefox" "")
                    ("Zathura" "📓")
                    ("Nsxiv" "⌧")
                    (_ ))
                  (or exwm-title "*EXWM*"))
          (string-remove-suffix " — Mozilla Firefox" it)
          (string-remove-suffix " — Mozilla Firefox Private Browsing" it)
          (string-remove-suffix " - YouTube" it))))
  (defun nagy-update-exwm-input-global-keys ()
    (interactive)
    ;; we have to use the internal variable instead of
    ;; `exwm-input-global-keys' do that we do not have to set the
    ;; command that it bounds to again ( even though we use it to
    ;; condition the insertion of the key into the list itself).
    (setq exwm-input--global-keys nil)
    (map-keymap
     (lambda (key _binding)
       (let* ((key-desc (single-key-description key))
              (case-fold-search nil)
              )
         ;; (when (string-prefix-p "s-" key-desc))
         (when (string-match-p
                (rx bos
                    (or (seq "s-" (any "a-z"))
                        (seq "s-" (any "A-Z"))
                        "s-<XF86Paste>"
                        "s-<prior>"
                        "s-<next>"
                        "s-<home>"
                        "s-<end>"
                        "s-SPC"      ;; find-file-home
                        "s-<return>" ;; eshell
                        "s-<escape>" ;; exwm-reset
                        "s-<f1>"
                        "s-<f5>" ;; screenshot
                        "s-="
                        "<XF86Explorer>"
                        "C-<XF86Explorer>"
                        "<XF86AudioLowerVolume>"
                        "<XF86AudioRaiseVolume>"
                        "<XF86MonBrightnessDown>"
                        "<XF86MonBrightnessUp>"
                        )
                    eos)
                key-desc)
           (let* ((key-vector (kbd key-desc))
                  (key-command (key-binding key-vector)))
             (when key-command
               (message "exwm: %s:%s" key-vector key-desc)
               (cl-pushnew key-vector exwm-input--global-keys))
             ))))
     (current-global-map))
    (exwm-input--update-global-prefix-keys))
  :demand t
  :custom
  (exwm-manage-force-tiling t)
  (exwm-workspace-number 1)
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  (exwm-manage-configurations '((t char-mode t)))
  ;; (exwm-input-prefix-keys '())
  (exwm-randr-workspace-monitor-plist (let ((i 0))
                                        (flatten-list (mapcar (lambda (el)
                                                                (prog1 (list i (alist-get 'name el))
                                                                  (cl-incf i)))
                                                              (display-monitor-attributes-list)))))
  ;; (exwm-randr-workspace-monitor-plist (map-into (apply #'vector (mapcar (lambda (el) (alist-get 'name el))
  ;;                                                                       (display-monitor-attributes-list)))
  ;;                                               'plist))
  :init
  ;; https://github.com/ch11ng/exwm/issues/889
  ;; Frame focus bug
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (setopt exwm-input-global-keys
          `((,(kbd "s-v") . nagy-emacs-split-window-right-and-focus)
            (,(kbd "s-s") . nagy-emacs-split-window-below-and-focus)
            (,(kbd "s-V") . nagy-emacs-split-window-right)
            (,(kbd "s-S") . nagy-emacs-split-window-below)
            (,(kbd "C-s-SPC") . buffer-new-of-region)
            (,(kbd "C-M-s-SPC") . buffer-new-of-kill)
            (,(kbd "s-<f12>") . +toggle-tab-bar-mode-from-frame)
            ;; bookmarks
            (,(kbd "s-ð") . ,(lambda () (interactive) (find-file "~/Downloads")))
            (,(kbd "C-s-j") . browse-url-from-kill)
            (,(kbd "H-f") . browse-url-at-point)
            (,(kbd "s-<backspace>") . nagy-move-tab-left)
            (,(kbd "S-s-<delete>") . nagy-move-tab-right)
            ))
  :config
  ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
  (add-hook 'exwm-update-class-hook #'nagy-exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook #'nagy-exwm-rename-buffer)
  (add-hook 'exwm-init-hook #'nagy-fix-frame)
  (evil-set-initial-state 'exwm-mode 'emacs)
  (require 'exwm-randr)
  (exwm-randr-mode 1)
  ;; (exwm-enable)
  (exwm-wm-mode)
  ;; (evil-define-key 'normal dired-mode-map "," #'terminal)
  ;; (keymap-set exwm-mode-map "s-j" #'nagy-url-kill)
  :bind
  ("s-I" . ibuffer-exwm)
  ("s-<escape>" . exwm-reset)
  )

(defun delete-window-or-tab (&optional WINDOW)
  (interactive)
  (if (= 1 (length (window-list)))
      (tab-close)
    (delete-window WINDOW)
    (balance-windows)))
(keymap-global-set "s-d" #'delete-window-or-tab)

;; (keymap-global-set "s-o" #'next-window-any-frame)
;; (keymap-global-set "s-H" #'evil-window-move-far-left)
;; (keymap-global-set "s-L" #'evil-window-move-far-right)
;; (keymap-global-set "s-J" #'evil-window-move-very-bottom)
;; (keymap-global-set "s-K" #'evil-window-move-very-top)

(defun start-terminal (&rest args)
  (declare (indent 0))
  (unless (display-graphic-p)
    (user-error "No display for terminal."))
  (apply #'start-process
         "terminal" nil "alacritty"
         "--option" (format "font.size=%d" (/  (face-attribute 'default :height) 9))
         args
         ))

(defun htop ()
  (interactive)
  (start-terminal "--title" "htop" "-e" "htop"))

(declare-function dayp "nagy-modus-themes")
(defun dool ()
  (interactive)
  (if (dayp)
      (start-terminal "--title" "dool" "-e" "dool" "--bytes" "-N" "wlp4s0" "--bw")
    (start-terminal "--title" "dool" "-e" "dool" "--bytes" "-N" "wlp4s0")))

(defvar terminal-number 1)

(defun terminal ()
  (interactive)
  (start-terminal "--title" (number-to-string (cl-incf terminal-number))))

(keymap-global-set "s-+" #'terminal)
;; (evil-global-set-key 'normal "." #'terminal)
(evil-global-set-key 'normal "," #'terminal)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "." #'terminal))

(defun nsxiv ()
  (interactive)
  (with-environment-variables
      (("XDG_CACHE_HOME" (concat temporary-file-directory "/xdg-cache")))
    (make-process :name "nsxiv"
                  :buffer nil
                  :connection-type 'pipe
                  :noquery t
                  :command '("nsxiv"
                             "--scale-mode" "f"
                             "--thumbnail"
                             "--no-bar"
                             ;; "--private"
                             "."
                             ))))
(keymap-global-set "<pause>" #'nsxiv)

(defun firefox ()
  (interactive)
  (start-process "firefox" nil browse-url-firefox-program "--new-window"))
(keymap-global-set "<XF86Explorer>" #'firefox)

(defun firefox-private-window ()
  (interactive)
  (start-process "firefox" nil browse-url-firefox-program "--private-window"))
(keymap-global-set "C-<XF86Explorer>" #'firefox-private-window)

(defun font-size-toggle ()
  (interactive)
  (alet (face-attribute 'default :height)
    (set-face-attribute 'default nil
                        :height (if (>= it 139) 100 140))))
;; (setopt split-window-preferred-function #'split-window-horizontally)

(keymap-global-set "H-<f1>" #'font-size-toggle)
(keymap-set evil-normal-state-map "<key-chord> - r" #'font-size-toggle)
(key-chord-register-keys ?- ?r)

(defun font-size-smol ()
  (interactive)
  (alet (face-attribute 'default :height)
    (set-face-attribute 'default nil
                        :height (if (>= it 100) 90 100))))
(keymap-global-set "H-<f12>" #'font-size-smol)

(provide 'nagy-exwm)
;;; nagy-exwm.el ends here
