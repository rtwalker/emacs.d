;;; rust.el --- Rust configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Rust development configuration

;;; Code:

(defface rust-unsafe-keyword-face
  `((t :family ,(if-let* ((font "PragmataPro Mono Liga Fraktur")
                          ((find-font (font-spec :name font))))
                    font
                  (face-attribute 'default :family))
       :weight bold
       :slant italic
       :foreground ,(doom-color 'red)))
  "Face for the `unsafe` keyword."
  :group 'rust)

(defface rust-unsafe-block-face
  `((((class color) (background light))
     :background ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.25) :extend t)
    (((class color) (background dark))
     :background ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.5) :extend t)
    (t
     :background ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.25) :extend t))
  "Face for `unsafe` block content."
  :group 'rust)

(defun rtw/rust-font-lock ()
  (face-remap-add-relative 'font-lock-type-face '(:slant normal))
  (when (treesit-ready-p 'rust)
    ;; make this a "level 3" feature
    (setq-local treesit-font-lock-feature-list
                `(,(car treesit-font-lock-feature-list)
                  ,(cadr treesit-font-lock-feature-list)
                  ,(append (nth 2 treesit-font-lock-feature-list) '(unsafe-block))
                  ,(nth 3 treesit-font-lock-feature-list)))

    (let ((unsafe-rules (treesit-font-lock-rules
                         :language 'rust
                         :feature 'keyword
                         :override 'append
                         '(("unsafe" @rust-unsafe-keyword-face))
                         :language 'rust
                         :feature 'unsafe-block
                         :override 'append
                         '((unsafe_block) @rust-unsafe-block-face))))

      (setq-local treesit-font-lock-settings
                  (append unsafe-rules treesit-font-lock-settings))

      (treesit-font-lock-recompute-features))))

(defun rtw/rust-font-lock-refresh (&rest _)
  (interactive)
  (face-spec-recalc 'rust-unsafe-block-face (selected-frame))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eq major-mode 'rust-mode)
                 (treesit-ready-p 'rust))
        (font-lock-flush)))))

(use-package rust-mode
  :after apheleia eglot
  :hook ((rust-mode . eglot-ensure)
         (rust-mode . #'rtw/rust-font-lock))
  :config
  (push '(rustfmt-nightly . ("rustfmt-nightly" "--quiet" "--emit" "stdout"))
        apheleia-formatters))

;;; rust.el ends here
