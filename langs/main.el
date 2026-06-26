;;; main.el --- Language configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for all language mode configurations.

;;; Code:

(use-package eglot
  :after eldoc project
  :custom
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(use-package flycheck)

(use-package flycheck-eglot
  :after (flycheck eglot))

(use-package treesit-fold
  :config
  (global-treesit-fold-mode))

(let ((langs-directory (expand-file-name "langs/" user-emacs-directory)))
  (dolist (file (directory-files langs-directory t "\\.el$"))
    (unless (string-match-p "main\\.el$" file)
      (load file))))

(use-package dockerfile-ts-mode
  :mode (("Dockerfile\\'" . dockerfile-ts-mode))
  :config
  (add-to-list 'treesit-language-source-alist
               '(dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))))

(use-package geiser)

(use-package idris-mode
  :custom
  (idris-interpreter-path "idris2"))

(use-package janet-mode
  :after apheleia
  :bind ( :map janet-mode-map
          ("C-c C-f" . apheleia-format-buffer))
  :config
  (unless (assoc 'janet-format apheleia-formatters)
    (push '(janet-format . ("janet-format" "--input" filepath))
          apheleia-formatters))
  (setf (alist-get 'janet-mode apheleia-mode-alist) 'janet-format)

  (use-package ajsc
    :hook (janet-mode . ajsc-interaction-mode)))

(use-package julia-mode)

(use-package just-ts-mode
  :config
  (add-to-list 'treesit-language-source-alist just-ts-mode-treesit-language-source))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode))

(use-package lua-ts-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :defer t
  :init (setq markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package morlock
  :config
  (add-hook 'emacs-lisp-mode-hook 'morlock-mode))

(use-package nickel-mode)

(use-package nix-ts-mode
  :after eglot
  :mode "\\.nix\\'"
  :config
  (add-to-list 'eglot-server-programs '((nix-ts-mode nix-mode) . ("nixd")))
  (add-to-list 'treesit-language-source-alist '(nix . ("https://github.com/nix-community/tree-sitter-nix")))
  :hook (nix-ts-mode . eglot-ensure))

(use-package python
  :after apheleia
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("ruff" "server")))
  (add-to-list 'treesit-language-source-alist '(python . ("https://github.com/tree-sitter/tree-sitter-python")))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setq python-prettify-symbols-alist nil)
  :hook (python-mode . eglot-ensure))

(use-package racket-mode
  :defer t)

(use-package raku-mode
  :config
  (setq raku-exec-path (home-manager-prefix "raku")))

(use-package scala-mode
  :interpreter "scala"
  :hook (scala-mode . eglot-ensure))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package tcl
  :custom
  (tcl-application "tclsh"))

(use-package toml-ts-mode
  :config
  (add-to-list 'treesit-language-source-alist '(toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))

(use-package yaml-pro
  :after yaml-ts-mode
  :bind (:map yaml-ts-mode-map
              ("C-M-n" . #'yaml-pro-ts-next-subtree)
              ("C-M-p" . #'yaml-pro-ts-prev-subtree)
              ("C-M-u" . #'yaml-pro-ts-up-level)
              ("C-M-d" . #'yaml-pro-ts-down-level)
              ("C-M-k" . #'yaml-pro-ts-kill-subtree)
              ("C-M-<backspace>" . #'yaml-pro-ts-kill-subtree)
              ("C-M-a" . #'yaml-pro-ts-first-sibling)
              ("C-M-e" . #'yaml-pro-ts-last-sibling))
  :hook
  ((yaml-mode yaml-ts-mode) . yaml-pro-mode))

(use-package yaml-ts-mode
  :config
  (add-to-list 'treesit-language-source-alist '(yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml.git"))))

(use-package zig-mode
  ;; :custom (zig-ast-check-on-format t)
  :defer t)

;;; main.el ends here
