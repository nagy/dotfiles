;;; wasm.el --- wasm mode -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; (require 'bindat)

;; https://blog.ttulka.com/learning-webassembly-2-wasm-binary-format/

;; (defconst wasm-module-bindat-spec
;;   `(("magic"    (bindat-type :uint32))
;;     ("version"  (bindat-type :uint32))
;;     ("sections" (bindat-type
;;                   (vec :uint32
;;                        (lambda ()
;;                          (let ((section-type (bindat-get "type")))
;;                            (cond ((equal section-type 0)
;;                                   `(("size" ,(bindat-type :uint32))
;;                                     ("name" ,(bindat-type :strz))))
;;                                  ((equal section-type 1)
;;                                   `(("size" ,(bindat-type :uint32))
;;                                     ("body" ,(bindat-type :vec :uint8))))
;;                                  ;; Add more section types here...
;;                                  (t nil)))))))))

(provide 'wasm)
;;; wasm.el ends here
