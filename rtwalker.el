;;; rtwalker.el --- user-init-file                    -*- lexical-binding: t -*-
(progn ;
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (load "pragmatapro-prettify-symbols-v0.830")
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

(use-package consult
  :bind
  (("M-s M-g" . consult-ripgrep)
   ("M-s M-f" . consult-fd)
   ("M-s M-o" . consult-outline)
   ("M-s M-l" . consult-line)
   ("M-s M-b" . consult-buffer)))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode))

(use-package dimmer
  :custom
  (dimmer-adjustment-mode :both)
  (dimmer-fraction 0.1)
  :config
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-configure-which-key)
  (dimmer-mode))

(use-package dired
  :custom
  (dired-maybe-use-globstar t))

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
  :after eldoc project
  :init
  (add-hook 'rust-mode-hook 'eglot-ensure)
  :custom
  (eglot-extend-to-xref t))

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))


(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(use-package embark
  :bind
  (("C-;" . embark-act)
   ("M-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  (evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init '(compile dired eglot ibuffer magit)))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package faces
  :config
  (set-face-attribute 'default nil :family "PragmataPro Mono" :height 160))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))
;;   :hook prog-mode)
;; 
;; (use-package flyspell
;;   :hook (org-mode text-mode))

(use-package gptel)

(use-package hideshow
  :config
  (add-hook 'rust-mode-hook #'hs-minor-mode))

(use-package hydra
  :bind ("M-z" . hydra-zoom/body)
  :config
  (defhydra hydra-zoom (:color amaranth)
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out")
    ("d" (text-scale-adjust 0) "default")
    ("0" (text-scale-adjust 0) "default")
    ("q" nil "quit" :color blue))

  (defhydra hydra-window (:color amaranth)
    ("w" (enlarge-window 2) "top")
    ("d" (enlarge-window 2 t) "left")
    ("s" (shrink-window 2) "bottom")
    ("a" (shrink-window 2 t) "right")
    ("b" (balance-windows) "balance" :color blue)
    ("q" nil "quit" :color blue)))


(use-package julia-mode
  :defer t)

(use-package kotlin-ts-mode
  :defer t
  :mode "\\.kt\\'")

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package markdown-mode
  :init (setq markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package minions
  :custom (minions-mode-line-lighter "&")
  :config
  (minions-mode))

(use-package nix-mode)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package racket-mode
  :defer t)

(use-package raku-mode
  :defer t)

(use-package rg
  :config
  (rg-enable-menu))

(use-package rust-mode
  :defer t
  :custom
  (rust-rustfmt-bin "~/.cargo/bin/rustfmt")
  (rust-format-on-save nil)
  (rust-indent-method-chain t)
  (rust-indent-where-clause t))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 20
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 10))
  (spacious-padding-mode 1))

(use-package tcl
  :custom
  (tcl-application "tclsh"))

(use-package treesit
  :config
  (add-to-list 'treesit-language-source-alist '(kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package window
  :config
  (defun is-portrait ()
    (> (frame-native-height) (frame-native-width)))
  (defun update-display-buffer-alist ()
    (setq display-buffer-alist
          `(("^\\*compilation"
             (display-buffer-in-side-window)
             (side . bottom)
             (window-height . 40)
             (reusable-frames . nil))
            ("^magit: "
             (display-buffer-reuse-window display-buffer-in-side-window)
             (side . ,(if (is-portrait) 'top 'left))
             (slot . 1)
	     ,(if (is-portrait)
		  '(window-height . 0.33)
		'(window-width . 0.33))
             (reusable-frames . nil))
            ("^\\*Help"
             (display-buffer-reuse-window display-buffer-in-side-window)
             (side . ,(if (is-portrait) 'bottom 'right))
             (slot . 1)
	     ,(if (is-portrait)
		  '(window-height . 0.33)
		'(window-width . 0.33))
             (reusable-frames . nil)))))
  (defun display-buffer-focus-change-function ()
    (when (frame-focus-state)
      (update-display-buffer-alist)))
  (add-function :after after-focus-change-function 'display-buffer-focus-change-function))

(use-package which-key
  :custom
  (which-key-allow-evil-operators 1)
  (which-key-idle-delay 0.1)
  :config
  (which-key-mode))

;;; Finishing touches

(use-package envrc
  :config
  (envrc-global-mode))

;;; rtwalker.el ends here
