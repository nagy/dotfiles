;;; nagy-exwm.el --- config emacs packages -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") anaphora evil-escape nagy-dired nagy-emacs nagy-url nagy-gc)

;; NIX-EMACS-PACKAGE: modus-themes
(require 'modus-themes)

;; NIX-EMACS-PACKAGE: dash
(require 'dash)
;; NIX-EMACS-PACKAGE: evil
(require 'evil)
(eval-when-compile
  (require 'dired))
(require 'nagy-dired)
(require 'key-chord)

(declare-function ibuffer-filter-by-used-mode "ibuffer")

;; For emacs 30. the upstream one now requires an event.
(defun nagy-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-this-buffer))
;; this does not work yet
;; (defalias 'nagy-kill-this-buffer (symbol-function 'kill-this-buffer))

;; TODO prevent rollover
(defun nagy-move-tab-right (arg)
  (interactive "P")
  (tab-bar-move-tab (or arg 1)))
(defun nagy-move-tab-left (arg)
  (interactive "P")
  (tab-bar-move-tab (* -1 (or arg 1))))

(defun nagy-next-window-any-frame ()
  "Do not want to move the mouse when switching windows, but switch
windows when moving the mouse."
  (interactive)
  (let ((mouse-autoselect-window nil)
        (focus-follows-mouse nil))
    (next-window-any-frame)))

(defun nagy-previous-window-any-frame ()
  "Do not want to move the mouse when switching windows, but switch
windows when moving the mouse."
  (interactive)
  (let ((mouse-autoselect-window nil)
        (focus-follows-mouse nil))
    (previous-window-any-frame)))

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
    (call-process "brightnessctl" nil nil nil
                  "--device=ddcci4" "set" (concat (number-to-string brightness--value) "%"))
    (call-process "brightnessctl" nil nil nil
                  "--device=ddcci12" "set" (concat (number-to-string brightness--value) "%"))
    (message "Brightness set to %d" brightness--value)))

(defun brightness-down ()
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-decf brightness--value 5)
    (setq brightness--value (max 0 brightness--value))
    (call-process "brightnessctl" nil nil nil
                  "--device=ddcci4" "set" (concat (number-to-string brightness--value) "%"))
    (call-process "brightnessctl" nil nil nil
                  "--device=ddcci12" "set" (concat (number-to-string brightness--value) "%"))
    (message "Brightness set to %d" brightness--value)))

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
                    (_ ))
                  (or exwm-title "*EXWM*"))
          (string-remove-suffix " — Mozilla Firefox" it)
          (string-remove-suffix " — Mozilla Firefox Private Browsing" it)
          (string-remove-suffix " - YouTube" it))))
  :demand t
  :custom
  (exwm-manage-force-tiling t)
  (exwm-workspace-number 1)
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  (exwm-manage-configurations '((t char-mode t)))
  ;; TODO use (display-monitor-attributes-list)
  (exwm-randr-workspace-monitor-plist '(0 "DP-1" 1 "HDMI-1"))
  :init
  ;; https://github.com/ch11ng/exwm/issues/889
  ;; Frame focus bug
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (setopt exwm-input-global-keys
          `((,(kbd "s-<escape>")  . exwm-reset)
            (,(kbd "s-m") . ,(key-binding (kbd "s-m")))
            (,(kbd "s-i") . nagy-previous-window-any-frame)
            (,(kbd "s-o") . nagy-next-window-any-frame)
            (,(kbd "s-<return>") . eshell)
            (,(kbd "s-p") . consult-bookmark)
            (,(kbd "<XF86Favorites>") . consult-bookmark)
            (,(kbd "s-U") . winner-redo)
            (,(kbd "s-u") . winner-undo)
            (,(kbd "s-d") . delete-window-or-tab)
            (,(kbd "s-v") . nagy-emacs-split-window-right-and-focus)
            (,(kbd "s-s") . nagy-emacs-split-window-below-and-focus)
            (,(kbd "s-V") . nagy-emacs-split-window-right)
            (,(kbd "s-S") . nagy-emacs-split-window-below)
            (,(kbd "s-I") . ibuffer-exwm)
            (,(kbd "s-<f1>") . calendar)
            (,(kbd "<XF86Explorer>") . firefox)
            (,(kbd "C-<XF86Explorer>") . firefox-private-window)
            (,(kbd "s-+") . terminal)
            (,(kbd "s-H") . evil-window-move-far-left)
            (,(kbd "s-J") . evil-window-move-very-bottom)
            (,(kbd "s-K") . evil-window-move-very-top)
            (,(kbd "s-L") . evil-window-move-far-right)
            (,(kbd "s-q") . bury-buffer)
            (,(kbd "s-Q") . unbury-buffer)
            (,(kbd "H-<f2>") . modus-themes-toggle)
            ;; (,(kbd "H-i") . xwininfo-from-buffer)
            (,(kbd "s-t") . find-tmp)
            (,(kbd "s-=") . balance-windows)
            (,(kbd "s-n") . universal-argument)
            (,(kbd "<XF86AudioMute>") . mute)
            (,(kbd "<XF86AudioLowerVolume>") . volume-decrease)
            (,(kbd "<XF86AudioRaiseVolume>") . volume-increase)
            (,(kbd "<XF86MonBrightnessUp>") . brightness-up)
            (,(kbd "<XF86MonBrightnessDown>") . brightness-down)
            (,(kbd "s-k") . nagy-kill-this-buffer) ; for emacs-30 . the upstream one now requires an event.
            (,(kbd "s-<XF86Paste>") . tab-new)
            (,(kbd "s-h") . tab-previous)
            (,(kbd "s-l") . tab-next)
            (,(kbd "s-f") . find-file)
            ;; (,(kbd "s-ø") . find-file-org)
            (,(kbd "s-<prior>") . tab-previous)
            (,(kbd "s-<next>") . tab-next)
            (,(kbd "s-<home>") . tab-first)
            (,(kbd "s-<end>") . tab-last)
            (,(kbd "s-SPC") . find-file-home)
            (,(kbd "C-s-SPC") . buffer-new-of-region)
            (,(kbd "C-M-s-SPC") . buffer-new-of-kill)
            (,(kbd "<XF86Back>") . tab-previous)
            (,(kbd "<XF86Forward>") . tab-next)
            (,(kbd "<XF86Search>") . other-frame)
            ;; (,(kbd "A-C-s-}") . tab-duplicate)
            (,(kbd "<f9>") . emms-pause)
            ;; (,(kbd "s-t") . foo-link)
            (,(kbd "s-<f11>") . global-hide-mode-line-mode)
            (,(kbd "s-<f12>") . +toggle-tab-bar-mode-from-frame)
            (,(kbd "H-M") . view-echo-area-messages)
            ;; bookmarks
            (,(kbd "s-ð") . ,(lambda () (interactive) (find-file "~/Downloads")))
            (,(kbd "s-j") . dired-jump)
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
  ("s-I" . ibuffer-exwm))

(defun delete-window-or-tab (&optional WINDOW)
  (interactive)
  (if (= 1 (length (window-list)))
      (tab-close)
    (delete-window WINDOW)
    (balance-windows)))

(keymap-global-set "s-d" #'delete-window-or-tab)
;; (keymap-global-set "s-o" #'next-window-any-frame)
(keymap-global-set "s-H" #'evil-window-move-far-left)
(keymap-global-set "s-L" #'evil-window-move-far-right)
(keymap-global-set "s-J" #'evil-window-move-very-bottom)
(keymap-global-set "s-K" #'evil-window-move-very-top)

(defun completing-read-host ()
  (completing-read "host> " nil))

(defun start-terminal (&rest args)
  (declare (indent 0))
  (unless (display-graphic-p)
    (user-error "No display for terminal."))
  (apply #'start-process
         "terminal" nil "alacritty"
         "--option" (format "font.size=%d" (/  (face-attribute 'default :height) 9))
         (mapcar #'shell-quote-argument args)))

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
