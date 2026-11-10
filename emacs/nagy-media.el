;;; nagy-media.el --- Multimedia configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") emms nagy-use-package)

(require 'general)

;; (use-package emms
;;   :defer t
;;   )

(use-package emms-player-mpv
  :defer t
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-player-mpv-environment '("PULSE_PROP_media.role=music"))
  (emms-player-mpv-parameters '("--quiet" "--really-quiet"
                                ;; "--no-config"
                                ;; "--save-position-on-quit"
                                "--no-audio-display"
                                "--mute=no"
                                "--force-window=no"
                                "--vo=null")))


;; (use-package emms-playlist-mode
;;   ;; :defer t
;;   ;; :config
;;   ;; :bind
;;   ;; ("<XF86AudioPause>" . emms)
;;   )

(use-package emms
  :defer t
  :custom
  ;; dont make the buffer hidden
  (emms-playlist-buffer-name "*EMMS Playlist*")
  :bind
  ("<XF86AudioPause>" . emms))


(defun mute ()
  "Silences/mutes the audio output via pulseaudio."
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-assert (zerop (call-process "pactl" nil nil nil "set-sink-mute" "alsa_output.pci-0000_c5_00.6.analog-stereo" "toggle")))))
(keymap-global-set "<XF86AudioMute>" #'mute)

(defun volume-increase ()
  "Louder."
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-assert (zerop (call-process "pactl" nil nil nil "set-sink-volume" "alsa_output.pci-0000_c5_00.6.analog-stereo" "+1000")))))
(keymap-global-set "<XF86AudioRaiseVolume>" #'volume-increase)

(defun volume-decrease ()
  "Silenter."
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-assert (zerop (call-process "pactl" nil nil nil "set-sink-volume" "alsa_output.pci-0000_c5_00.6.analog-stereo" "-1000")))))
(keymap-global-set "<XF86AudioLowerVolume>" #'volume-decrease)

;; NIX-EMACS-PACKAGE: elfeed
(use-package elfeed
  :preface
  (defvar elfeed-show-mode-hook nil)
  :defer t
  :custom
  (elfeed-show-truncate-long-urls nil)
  (elfeed-search-filter "+unread")
  (elfeed-search-title-max-width 100)
  (elfeed-curl-max-connections 1)
  :config
  (put 'elfeed-search-bookmark-handler 'bookmark-handler-type "Elfeed Search")
  ;; :bind
  ;; (:map elfeed-show-mode-map
  ;;       ("SPC" . nil))
  :general
  (:states 'normal :keymaps 'elfeed-search-mode-map
           "SPC" nil
           "r" #'elfeed-search-untag-unread
           "↓" #'elfeed-search-fetch)
  ;; (:states 'normal :keymaps 'elfeed-show-mode-map
  ;;          "SPC" nil)
  (:keymaps 'elfeed-search-mode-map
            [remap kill-this-buffer] #'elfeed-db-unload
            [remap save-kill-buffer] #'elfeed-db-unload
            [remap nagy-kill-this-buffer] #'elfeed-db-unload))

(provide 'nagy-media)
;;; nagy-media.el ends here
