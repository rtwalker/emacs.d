;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name (convert-standard-filename "var/eln-cache/")
                            user-emacs-directory)))

(setq load-prefer-newer t)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "lib/auto-compile" dir))
  (add-to-list 'load-path (expand-file-name "lib/gcmh" dir)))

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(require 'gcmh)
(gcmh-mode 1)

(setq package-enable-at-startup nil)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               (cons "melpa" "https://melpa.org/packages/")
               t))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
