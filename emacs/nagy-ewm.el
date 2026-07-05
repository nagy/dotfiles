;;; nagy-ewm.el --- Wayland compositor via EWM -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; NIX-EMACS-PACKAGE: ewm
(use-package ewm
  :if (getenv "WAYLAND_DISPLAY")
  :commands (ewm-start-module ewm--send-intercept-keys)
  :custom
  (ewm-animations-enabled nil)
  (ewm-intercept-prefixes
   '(?\M-x
     (?\s-f :fullscreen)
     ("<MonBrightnessUp>" :fullscreen) ("<MonBrightnessDown>" :fullscreen)
     ("<AudioRaiseVolume>" :fullscreen) ("<AudioLowerVolume>" :fullscreen)
     ("<AudioMute>" :fullscreen) ("<AudioMicMute>" :fullscreen)
     ("<Print>" :fullscreen)))
  ;; :quelpa (ewm :fetcher git :url "https://codeberg.org/ezemtsov/ewm.git")
  ;; EWM runs from TTY like EXWM, but requires a PGTK Emacs build.
  :config
  (ewm-start-module)
  ;; Add intercept keybindings AFTER startup so we don't crash
  ;; the compositor during initialization.
  (keymap-set ewm-mode-map "s-k" 'nagy-kill-this-buffer)
  (keymap-set ewm-mode-map "s-j" 'dired-jump)
  (keymap-set ewm-mode-map "s-o" 'nagy-next-window-any-frame)
  (keymap-set ewm-mode-map "s-i" 'nagy-previous-window-any-frame)
  (ewm--send-intercept-keys))


(provide 'nagy-ewm)
;;; nagy-ewm.el ends here
