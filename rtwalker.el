;;; rtwalker.el --- user-init-file                    -*- lexical-binding: t -*-
(progn ;
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (load "pragmatapro-prettify-symbols-v0.829")
  (add-hook 'prog-mode-hook #'prettify-hook))

(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package apheleia
  :config (apheleia-global-mode +1))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1))

(use-package avy
  :bind
  ("M-j" . 'avy-goto-char-timer))

(use-package calc
  :defer t
  :bind ("M-+" . calc))

(use-package calendar
  :bind ("M-C" . calendar)
  :custom
  (calendar-week-start-day 1)
  :hook
  (calendar-today-visible . calendar-mark-today))

(use-package display-line-numbers
  :config
  (defun show-line-numbers-mode ()
    (setq display-line-numbers 'relative)
    (setq display-line-numbers-width 4))
  (add-hook 'prog-mode-hook 'show-line-numbers-mode)
  (set-face-attribute 'line-number-current-line nil :foreground "#ee7621")
  (set-face-attribute 'line-number-current-line nil :background "#f0f0f1"))

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-day t))

(use-package eglot
  :after eldoc flymake project
  :custom
  (eglot-extend-to-xref t))

(use-package eshell
  :defer t
  :config
  (setq eshell-prompt-header "\n┌─ ")
  (setq eshell-prompt-regexp "└─>> ")
  (setq eshell-prompt-string "└─>> ")
  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat eshell-prompt-header
            "\xf115 "
            (abbreviate-file-name (eshell/pwd))
            "\n"
            eshell-prompt-string))
  (setq eshell-prompt-function 'esh-prompt-func))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-mode-line-format nil)
  (evil-insert-state-message nil)
  :config
  (evil-mode 1)
  (add-to-list 'evil-emacs-state-modes 'special-mode))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init '(compile dired eglot ibuffer magit)))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package faces
  :config
  (set-face-attribute 'default nil :family "PragmataPro Mono" :height 130))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :hook prog-mode)

(use-package flyspell
  :hook (org-mode text-mode))

(use-package markdown-mode
  :init (setq markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package window
  :config
  (setq display-buffer-alist
        `(("^\\*compilation"
           (display-buffer-in-side-window)
           (side . bottom)
           (window-height . 40)
           (reusable-frames . nil))
          ("^magit: "
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . left)
           (slot . 1)
           (window-width . 0.33)
           (reusable-frames . nil))
          ("^\\*Help"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (slot . 1)
           (window-width . 0.33)
           (reusable-frames . nil)))))

(use-package which-key
  :custom
  (which-key-allow-evil-operators 1)
  (which-key-idle-delay 0.1)
  :config
  (which-key-mode)
  (which-key-show-major-mode))
