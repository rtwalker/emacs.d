;;; main.el --- Language configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for all language mode configurations.

;;; Code:

(require 'eglot)

(let ((langs-directory (expand-file-name "langs/" user-emacs-directory)))
  (dolist (file (directory-files langs-directory t "\\.el$"))
    (unless (string-match-p "main\\.el$" file)
      (load file))))

(use-package julia-mode
  :defer t)

(use-package just-ts-mode
  :config
  (add-to-list 'treesit-language-source-alist just-ts-mode-treesit-language-source))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode))

(use-package markdown-mode
  :defer t
  :init (setq markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package morlock
  :config
  (add-hook 'emacs-lisp-mode-hook 'morlock-mode))

(use-package nix-mode
  :defer t
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  (nix-mode . eglot-ensure))

(use-package python
  :after apheleia
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("ruff" "server")))
  (add-to-list 'treesit-language-source-alist '(python . ("https://github.com/tree-sitter/tree-sitter-python")))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
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

(use-package zig-mode
  ;; :custom (zig-ast-check-on-format t)
  :defer t)

;;; main.el ends here
