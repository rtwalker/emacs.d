;;; rust.el --- Rust configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Rust development configuration

;;; Code:

(use-package flycheck-rust)

(use-package rust-ts-mode
  :config
  (setq rust-ts-mode-prettify-symbols-alist nil)

  (defface rust-unsafe-keyword-face
    `((t :family ,(if-let* ((font "PragmataPro Mono Liga Fraktur")
                            ((find-font (font-spec :name font))))
                      font
                    (face-attribute 'default :family))
         :weight bold
         :slant italic
         :inherit warning))
    "Face for the `unsafe` keyword."
    :group 'rust)

  (defface rust-unsafe-block-face
    `((t :background ,(modus-themes-get-color-value 'bg-red-nuanced) :extend t :foreground unspecified))
    "Face for `unsafe` block content."
    :group 'rust)

  (defun rtw/rust-ts-font-lock ()
    (when (treesit-ready-p 'rust)
      ;; make "unsafe-block" a "level 3" feature
      (setq-local treesit-font-lock-feature-list
                  '(( comment definition)
                    ( keyword string)
                    ( assignment attribute builtin constant escape-sequence
                      number type unsafe-block )
                    ( bracket delimiter error function operator property variable)))
      (let ((unsafe-rules
             (treesit-font-lock-rules
              :language 'rust :feature 'keyword :override t '(("unsafe" @rust-unsafe-keyword-face))
              :language 'rust :feature 'unsafe-block :override 'keep "(unsafe_block) @rust-unsafe-block-face")))
        (setq-local treesit-font-lock-settings
                    (append unsafe-rules treesit-font-lock-settings))
        (treesit-font-lock-recompute-features))))

  (add-hook 'rust-ts-mode-hook #'rtw/rust-ts-font-lock)

  (defun rtw/rust-ts-imenu ()
    (when (treesit-ready-p 'rust)
      (setq-local treesit-simple-imenu-settings
                  `(("Module" "\\`mod_item\\'" nil nil)
                    ("Enum" "\\`enum_item\\'" nil nil)
                    ("Impl Trait"
                     "\\`impl_item\\'"
                     (lambda (node)
                       (treesit-node-child-by-field-name node "trait"))
                     nil)
                    ("Impl"
                     "\\`impl_item\\'"
                     (lambda (node)
                       (not (treesit-node-child-by-field-name node "trait")))
                     nil)
                    ("Type" "\\`type_item\\'" nil nil)
                    ("Struct" "\\`struct_item\\'" nil nil)
                    ("Fn" "\\`function_item\\'" nil nil)
                    ("Macro" "\\`macro_definition\\'" nil nil)
                    ("Trait" "\\`trait_item\\'" nil nil)
                    ("Union" "\\`union_item\\'" nil nil)))))

  (add-hook 'rust-ts-mode-hook #'rtw/rust-ts-imenu)

  (add-to-list 'treesit-language-source-alist '(rust . ("https://github.com/tree-sitter/tree-sitter-rust.git"))))

(use-package rust-mode
  :after (apheleia consult consult-imenu eglot flycheck flycheck-eglot flycheck-rust rust-ts-mode)
  :init (setq rust-mode-treesitter-derive t)
  :bind (:map rust-mode-map
              ("C-c C-m" . #'rtw/find-cargo-toml))
  :config
  (setq rust-mode-prettify-symbols-alist nil)

  (defun rtw/find-cargo-toml ()
    (interactive)
    (find-file-other-frame (concat (rust-buffer-crate) "Cargo.toml")))

  (setf (flycheck-checker-get 'rust-clippy 'enabled)
        (lambda ()
          (and (funcall flycheck-executable-find "cargo-clippy")
               (flycheck-rust-manifest-directory))))
  (setf (flycheck-checker-get 'rust-clippy 'verify)
        (lambda (_)
          (and buffer-file-name
               (let ((has-toml (flycheck-rust-manifest-directory))
                     (has-clippy (funcall flycheck-executable-find "cargo-clippy")))
                 (list
                  (flycheck-verification-result-new
                   :label "Clippy"
                   :message (if has-clippy "Found"
                              "Cannot find the `cargo clippy' command")
                   :face (if has-clippy 'success '(bold warning)))
                  (flycheck-verification-result-new
                   :label "Cargo.toml"
                   :message (if has-toml "Found" "Missing")
                   :face (if has-toml 'success '(bold warning))))))))

  (defun rtw/rust-mode-hook ()
    (interactive)
    (setq flycheck-eglot-exclusive nil)
    (eglot-ensure)
    (flycheck-mode)
    (flycheck-rust-setup)
    (flycheck-eglot-mode 1)
    (setq-local flycheck-checker 'eglot-check)
    (flycheck-add-next-checker 'eglot-check 'rust-cargo)
    (flycheck-add-next-checker 'rust-cargo '(warning . rust-clippy)))
  (add-hook 'rust-mode-hook  #'rtw/rust-mode-hook)

  (unless (assoc 'rustfmt-nightly apheleia-formatters)
    (push '(rustfmt-nightly . ("rustfmt-nightly" "--quiet" "--emit" "stdout"))
          apheleia-formatters))

  (add-to-list 'consult-imenu-config
               '((rust-mode :types
                            ((?f "Fn" font-lock-function-name-face)
                             (?e "Enum" font-lock-type-face)
                             (?i "Impl" font-lock-type-face)
                             (?I "Impl Trait" font-lock-type-face)
                             (?m "Macro" font-lock-preprocessor-face)
                             (?M "Module" font-lock-constant-face)
                             (?s "Struct" font-lock-type-face)
                             (?t "Trait" font-lock-type-face)
                             (?T "Type" font-lock-type-face)
                             (?u "Union" font-lock-type-face))))))

;;; rust.el ends here
