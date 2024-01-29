;;; nagy-exwm.el --- config emacs packages -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") evil exwm)

(require 'evil)
(require 'exwm)
(require 'exwm-randr)

(require 'ibuffer)

(declare-function GC-DISABLE "nagy-gc")
(declare-function evil-escape-mode "evil-escape")
(declare-function gcmh-mode "gcmh")
(declare-function modus-themes-toggle "modus-themes")


(defun brightness-up ()
  (interactive)
  (let ((default-directory "~/"))
    (cl-assert (zerop (call-process "brightnessctl" nil nil nil "--device=ddcci5" "set" "+5%")))
    (cl-assert (zerop (call-process "brightnessctl" nil nil nil "--device=ddcci6" "set" "+5%")))))

(defun brightness-down ()
  (interactive)
  (let ((default-directory "~/"))
    (cl-assert (zerop (call-process "brightnessctl" nil nil nil "--device=ddcci5" "set" "5%-")))
    (cl-assert (zerop (call-process "brightnessctl" nil nil nil "--device=ddcci6" "set" "5%-")))))
(use-package exwm
  :if (display-graphic-p)
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
    (let  ((it (car (frame-list))))
      (make-frame)
      (delete-frame it)
      (make-frame)
      (global-hl-line-mode -1)
      (modus-themes-toggle)
      (modus-themes-toggle)
      (when (fboundp 'gcmh-mode)
        (gcmh-mode -1))
      (global-hl-line-mode -1)
      (when (fboundp 'evil-escape-mode)
        (evil-escape-mode -1))
      (set-face-attribute 'default nil :height 120)
      (exwm-randr-refresh)
      (blink-cursor-mode -1)
      (evil-mode)
      (GC-DISABLE)
      ;; (font-size-toggle)
      (setq-default lexical-binding t)))
  (defun nagy-exwm-rename-buffer ()
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":" exwm-title)))
  :demand t
  :custom
  (exwm-manage-force-tiling t)
  (exwm-workspace-number 1)
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  (exwm-manage-configurations '((t char-mode t)))
  ;; TODO use (display-monitor-attributes-list)
  (exwm-randr-workspace-monitor-plist '(0 "DP-1" 1 "DP-2"))
  :init
  ;; https://github.com/ch11ng/exwm/issues/889
  ;; Frame focus bug
  (setq x-no-window-manager t)    ; did not work, but made frame focusing better
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (setopt exwm-input-global-keys
          `((,(kbd "s-<escape>")  . exwm-reset)
            (,(kbd "s-m") . ,(key-binding (kbd "s-m")))
            (,(kbd "s-i") . previous-window-any-frame)
            (,(kbd "s-o") . next-window-any-frame)
            (,(kbd "s-<return>") . eshell)
            (,(kbd "s-p") . consult-bookmark)
            (,(kbd "<XF86Favorites>") . consult-bookmark)
            (,(kbd "s-U") . winner-redo)
            (,(kbd "s-u") . winner-undo)
            ;; (,(kbd "M-”") . new-empty-buffer)
            (,(kbd "s-d") . delete-window-or-tab)
            (,(kbd "s-v") . nagy-emacs-split-window-right-and-focus)
            (,(kbd "s-s") . nagy-emacs-split-window-below-and-focus)
            ;; (,(kbd "s-TAB") . bufler-list)
            ;; (,(kbd "s-z") . save-kill-buffer)
            (,(kbd "s-I") . ibuffer-exwm)
            (,(kbd "s-<f1>") . calendar)
            (,(kbd "<XF86Explorer>") . firefox)
            (,(kbd "s-+") . terminal)
            (,(kbd "s-H") . evil-window-move-far-left)
            (,(kbd "s-J") . evil-window-move-very-bottom)
            (,(kbd "s-K") . evil-window-move-very-top)
            (,(kbd "s-L") . evil-window-move-far-right)
            (,(kbd "s-q") . bury-buffer)
            (,(kbd "s-Q") . unbury-buffer)
            (,(kbd "H-<f2>") . modus-themes-toggle)
            (,(kbd "s-t") . find-tmp)
            (,(kbd "s-⧖") . find-tmp)
            (,(kbd "s-=") . balance-windows)
            (,(kbd "s-n") . universal-argument)
            (,(kbd "<XF86AudioMute>") . mute)
            (,(kbd "<XF86AudioLowerVolume>") . volume-decrease)
            (,(kbd "<XF86AudioRaiseVolume>") . volume-increase)
            (,(kbd "<XF86MonBrightnessUp>") . brightness-up)
            (,(kbd "<XF86MonBrightnessDown>") . brightness-down)
            (,(kbd "s-k") . kill-this-buffer)
            (,(kbd "s-<XF86Paste>") . tab-new)
            (,(kbd "C-s-<XF86Paste>") . tab-new-find-file)
            (,(kbd "s-h") . tab-previous)
            (,(kbd "s-l") . tab-next)
            (,(kbd "s-<prior>") . tab-previous)
            (,(kbd "s-<next>") . tab-next)
            (,(kbd "s-<home>") . tab-first)
            (,(kbd "s-<end>") . tab-last)
            (,(kbd "<XF86Back>") . tab-previous)
            (,(kbd "<XF86Forward>") . tab-next)
            ;; (,(kbd "<XF86AudioPause>") . emms-pause)
            ;; (,(kbd "S-<XF86AudioPlay>") . emms-pause)
            ;; (,(kbd "s-<SunFront>") . start-firefox-browser)
            ;; (,(kbd "s-f") . find-file)
            (,(kbd "s-<f9>") . toggle-tool-bar-mode-from-frame)
            (,(kbd "s-<f10>") . toggle-menu-bar-mode-from-frame)
            (,(kbd "s-<f11>") . global-hide-mode-line-mode)
            (,(kbd "s-<f12>") . +toggle-tab-bar-mode-from-frame)
            (,(kbd "H-M") . view-echo-area-messages)))
  :config
  ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
  (add-hook 'exwm-update-class-hook #'nagy-exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook #'nagy-exwm-rename-buffer)
  (add-hook 'exwm-init-hook #'nagy-fix-frame)
  (evil-set-initial-state 'exwm-mode 'emacs)
  (require 'exwm-randr)
  (exwm-randr-enable)
  (exwm-enable)
  :bind
  ("s-I" . ibuffer-exwm))

(defun delete-window-or-tab (&optional WINDOW)
  (interactive)
  (if (= 1 (length (window-list)))
      (tab-close)
    (delete-window WINDOW)
    (balance-windows)))

(keymap-global-set "s-d" #'delete-window-or-tab)
(keymap-global-set "s-o" #'next-window-any-frame)
(keymap-global-set "s-H" #'evil-window-move-far-left)
(keymap-global-set "s-L" #'evil-window-move-far-right)
(keymap-global-set "s-J" #'evil-window-move-very-bottom)
(keymap-global-set "s-K" #'evil-window-move-very-top)

(provide 'nagy-exwm)
;;; nagy-exwm.el ends here
