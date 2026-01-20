;;; rtwalker.el --- user-init-file                    -*- lexical-binding: t -*-
(progn
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (setq backup-inhibited nil))

(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package apheleia
  :config (apheleia-global-mode +1))

(use-package auth-source
  :config (setq auth-sources '("~/.authinfo")))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1))

(use-package avy
  :bind
  ("M-j" . 'avy-goto-char-timer))

(use-package biblio)

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
  :demand t
  :bind
  (("M-s M-g" . consult-ripgrep)
   ("M-s M-f" . consult-fd)
   ("M-s M-o" . consult-outline)
   ("M-s M-i" . consult-imenu)
   ("M-s M-I" . consult-imenu-multi)
   ("M-s M-l" . consult-line)
   ("M-s M-b" . consult-buffer)))

(use-package consult-imenu)

(use-package consult-xref
  :config
  (setq xref-show-xrefs-function #'consult-xref))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode))

(use-package corfu-indexed
  :after corfu
  :config (corfu-indexed-mode))

(use-package corfu-popupinfo
  :after corfu
  :config
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay '(1.0 . 0.5))
  (setq corfu-popupinfo-max-height 30))

(use-package corfu-quick
  :after corfu
  :bind (:map corfu-map
              ("M-q" . #'corfu-quick-complete)
              ("C-q" . #'corfu-quick-insert)))

(use-package dimmer
  :custom
  (dimmer-adjustment-mode :both)
  (dimmer-fraction 0.1)
  :config
  (defun rtw/dimmer-filter-more (faces)
    (remove 'mode-line-inactive faces))
  (advice-add 'dimmer-filtered-face-list :filter-return
              #'rtw/dimmer-filter-more)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-configure-which-key)
  (dimmer-mode))

(use-package difftastic
  :custom
  (difftastic-executable (home-manager-prefix "difft")))

(use-package display-line-numbers
  :config
  (defun show-line-numbers-mode ()
    (setq display-line-numbers 'relative)
    (setq display-line-numbers-width 4))
  :hook
  ((prog-mode text-mode) . #'show-line-numbers-mode))

(use-package ediff
  :config
  (setq ediff-split-window-function #'split-window-horizontally)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package eglot
  :after eldoc project
  :custom
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

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
  :demand t
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
  (setq eshell-prompt-string "└─>> ")
  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat eshell-prompt-header
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
  (evil-collection-init '(compile difftastic dired eglot forge hideshow ibuffer info magit org org-present vertico)))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package faces
  :config
  (set-face-attribute 'default nil :family "PragmataPro Mono Liga Serif" :height 160))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :hook prog-mode)

;; (use-package flyspell
;;   :hook (org-mode text-mode))

(use-package forge
  :after magit)

(use-package gptel)

(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(use-package hydra
  :bind
  (("M-h" . nil)
   ("M-h w" . hydra-window/body)
   ("M-h z" . hydra-zoom/body))
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
    ("a" (enlarge-window 2 t) "left")
    ("s" (shrink-window 2) "bottom")
    ("d" (shrink-window 2 t) "right")
    ("b" (balance-windows) "balance" :color blue)
    ("q" nil "quit" :color blue)))


(use-package ligature
  :config
  (setq pragmatapro-ligatures-alist
        (--remove (s-equals? "" it)
                  (s-split "\n"
                           (f-read (concat user-emacs-directory "assets/all-ligatures.txt")))))
  (ligature-set-ligatures 'prog-mode pragmatapro-ligatures-alist)
  (global-ligature-mode t))

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

(use-package minions
  :custom (minions-mode-line-lighter "&")
  :config
  (minions-mode))

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (string green-cooler)
          (comment yellow-cooler)
          (fg-line-number-active slate)
          (bg-line-number-active bg-hl-line)))
  (defun my-modus-themes-faces (&rest _)
    (custom-set-faces
     `(font-lock-comment-face ((t :background ,(modus-themes-get-color-value 'bg-yellow-nuanced))))
     `(font-lock-doc-face ((t :background ,(modus-themes-get-color-value 'bg-blue-nuanced) :weight semibold)))
     `(font-lock-string-face ((t :background ,(modus-themes-get-color-value 'bg-green-nuanced))))
     `(vertico-posframe-border ((t :background ,(modus-themes-get-color-value 'bg-dim))))
     `(vertico-quick1 ((t :inherit modus-themes-completion-match-0 :background ,(modus-themes-get-color-value 'bg-blue-nuanced))))
     `(vertico-quick2 ((t :inherit modus-themes-completion-match-1 :background ,(modus-themes-get-color-value 'bg-magenta-nuanced))))))
  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-faces)
  (modus-themes-load-theme 'modus-operandi-tinted))

(use-package nerd-icons
  :config
  (setq nerd-icons-font-family "PragmataPro Mono Liga"))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :preface
  (defun rtw/nerd-icons-icon-for-file (file)
    (nerd-icons-icon-for-file file :height 1.25))
  (defun rtw/nerd-icons-icon-for-dir (dir)
    (nerd-icons-icon-for-dir dir :height 1.25))
  :custom
  (nerd-icons-dired-file-icon-function #'rtw/nerd-icons-icon-for-file)
  (nerd-icons-dired-dir-icon-function #'rtw/nerd-icons-icon-for-dir)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org-present
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "*scratch*"
          help-mode
          eshell-mode
          comint-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

(use-package posframe
  :config
  (defun rtw/posframe-poshandler-frame-almost-top-center (info)
    "A custom posframe position handler.

Designed to be in between `posframe-poshandler-frame-center' and
`posframe-poshandler-frame-top-center'.

The structure of INFO can be found in docstring of
`posframe-show'."
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (/ (- (plist-get info :parent-frame-height)
                (plist-get info :posframe-height))
             4))))

(use-package rg
  :config
  (rg-enable-menu)
  (setq rg-executable (home-manager-prefix "rg")))

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

(use-package timeclock
  :custom
  (timeclock-file "~/time/log"))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :bind (:map vertico-map
              ("M-n" . #'vertico-next-group)
              ("M-p" . #'vertico-previous-group)))

(use-package vertico-buffer
  :after vertico
  :config
  (setq vertico-buffer-display-action 'display-buffer-reuse-window))

(use-package vertico-indexed
  :after (vertico vertico-reverse)
  :config (vertico-indexed-mode)
  (defun rtw/vertico-indexed-add-space (args)
    (if (and vertico-indexed-mode
             (bound-and-true-p vertico-indexed--min))
        (seq-let [cand prefix &rest rest] args
          `(,cand ,(concat " " prefix) ,@rest))
      args))
  (advice-add 'vertico--format-candidate :filter-args
              #'rtw/vertico-indexed-add-space))

(use-package vertico-multiform
  :commands vertico-multiform-mode
  :after vertico
  :init (vertico-multiform-mode 1)
  :config
  (setq vertico-multiform-categories
        '((imenu buffer)
          (t reverse)))
  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (consult-imenu-multi buffer)
          (consult-line buffer)
          (consult-ripgrep buffer)
          (execute-extended-command posframe)
          (t reverse))))

(use-package vertico-posframe
  :after (vertico posframe)
  :commands vertico-multiform-posframe
  :config
  (setq vertico-posframe-poshandler #'rtw/posframe-poshandler-frame-almost-top-center)
  (setq vertico-posframe-border-width 5)
  (setq vertico-posframe-min-width 120)
  (setq vertico-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10))))

(use-package vertico-quick
  :after (vertico embark)
  :bind (:map vertico-map
              ("M-i" . #'vertico-quick-insert)
              ("C-'" . #'vertico-quick-exit)
              ("M-'" . #'vertico-quick-embark))
  :config
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg))))

(use-package vertico-reverse
  :after vertico)

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 160)
  (visual-fill-column-center-text t))

(use-package window
  :config
  (defun is-portrait ()
    (> (frame-native-height) (frame-native-width)))
  (defun update-display-buffer-alist ()
    (setq display-buffer-alist
          `(("^magit: "
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

(use-package wgrep)

;;; Host- and Language-specific configurations

(dolist (file-name `(,(concat "hosts/" (system-name) ".el") "langs/main.el"))
  (let ((file (expand-file-name file-name user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;;; Finishing touches

(use-package envrc
  :init (setq envrc-debug t)
  :hook (after-init . envrc-global-mode)
  :config (envrc-global-mode))

;;; rtwalker.el ends here
