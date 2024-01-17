;;; nagy-media.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Homepage: https://github.com/nagy/nagy-media
;; Package-Requires: ((emacs "29.1") emms general nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)
(require 'nagy-use-package)

(use-package emms)

(use-package emms-player-mpv
  :config
  (setq emms-player-list '(emms-player-mpv)
        emms-player-mpv-environment '("PULSE_PROP_media.role=music")
        emms-player-mpv-parameters '("--quiet" "--really-quiet"
                                     ;; "--no-config"
                                     ;; "--save-position-on-quit"
                                     "--no-audio-display"
                                     "--mute=no"
                                     ;; "--force-window=no"
                                     ;; "--vo=null"
                                     )))

(use-package emms-playlist-mode
  ;; :defer t
  ;; :config
  :bind
  ("<XF86AudioPause>" . emms))

(provide 'nagy-media)
;;; nagy-media.el ends here
