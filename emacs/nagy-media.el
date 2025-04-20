;;; nagy-media.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") emms general nagy-use-package)

(require 'general)
;; (require 'nagy-use-package)

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
                                "--vo=null"
                                )))

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
  ("<XF86AudioPause>" . emms)
  )

(defun mute ()
  "Silences/mutes the audio output via pulseaudio."
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-assert (zerop (call-process "pactl" nil nil nil "set-sink-mute" "alsa_output.pci-0000_c5_00.6.analog-stereo" "toggle")))))

(defun volume-increase ()
  "Louder."
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-assert (zerop (call-process "pactl" nil nil nil "set-sink-volume" "alsa_output.pci-0000_c5_00.6.analog-stereo" "+1000")))))

(defun volume-decrease ()
  "Silenter."
  (interactive)
  (let ((default-directory temporary-file-directory))
    (cl-assert (zerop (call-process "pactl" nil nil nil "set-sink-volume" "alsa_output.pci-0000_c5_00.6.analog-stereo" "-1000")))))

(provide 'nagy-media)
;;; nagy-media.el ends here
