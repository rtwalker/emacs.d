;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  ;;(setq inhibit-startup-screen t)
  ;;(setq inhibit-startup-echo-area-message "locutus")
  ;;(setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (blink-cursor-mode 0)
  (global-hl-line-mode 1)
  (setq ring-bell-function #'ignore)
  (setq-default truncate-lines t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file null-device))

(use-package server
  :config (or (server-running-p) (server-mode)))

(use-package use-package-hydra)


;;; Long tail

(use-package ace-link
  :bind
    (:map org-mode-map
          ("M-O" . ace-link-org))
  :config
  (ace-link-setup-default))

(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ag)

(use-package all-the-icons)

;; borrowed from jabranham's init.el as a starting point
;; auctex
(use-package tex-site
  ;; AuCTeX is better than the built in tex mode; let's use it.
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :custom
  (TeX-lisp-directory (expand-file-name "auctex/" borg-drone-directory))
  (TeX-data-directory (expand-file-name "auctex/" borg-drone-directory))
  (TeX-auto-save t)
  (TeX-electric-sub-and-superscript t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  (TeX-source-correlate-mode t)
  (TeX-clean-confirm nil)
  ;; TeX-command-list by default contains a bunch of stuff I'll never
  ;; use. I use latexmk, xelatexmk, and View.  That's pretty much it.
  ;; Maybe one day I'll add "clean" back to the list.
  (TeX-command-list
   '(("latexmk" "latexmk -synctex=1 -quiet -pdf %s"
      TeX-run-compile nil t :help "Process file with latexmk")
     ("View" "%V" TeX-run-discard-or-function nil t :help "Run Viewer")
     ("xelatexmk" "latexmk -synctex=1 -quiet -xelatex %s"
      TeX-run-compile nil t :help "Process file with xelatexmk")
     ("Clean" "TeX-clean" TeX-run-function nil t
      :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t
      :help "Delete generated intermediate and output files")))
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . reftex-mode)
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . cdlatex-mode)
  :init
  (use-package tex-mode)
  :config
  (setq-default TeX-command-default "latexmk")
  ;; revert pdf from file after compilation finishes
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (use-package latex
    :bind
    (:map LaTeX-mode-map
          ("M-p" . outline-previous-visible-heading)
          ("M-n" . outline-next-visible-heading)
          ("<backtab>" . org-cycle))))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1))

(use-package calc
  :defer t
  :bind ("M-+" . calc))

(use-package calendar
  :custom
  (calendar-week-start-day 1)
  :hook
  (calendar-today-visible . calendar-mark-today))

(use-package calfw
  :config
  (use-package calfw-org))

(use-package company
  :config
  (set-face-attribute 'company-tooltip nil :background "#e3e3e5" :foreground "#87888d")
  (set-face-attribute 'company-tooltip-annotation nil :foreground "#a626a4")
  (set-face-attribute 'company-scrollbar-fg nil :background "#383a42")
  (set-face-attribute 'company-scrollbar-bg nil :background "#e3e3e5")
  (set-face-attribute 'company-tooltip-common nil :foreground "#383a42")
  (set-face-attribute 'company-tooltip-selection nil :background "#cce6f1")
  (global-company-mode 1))

(use-package company-math                                                                                                                          
  :config                                                                                                                                          
  (add-to-list 'company-backends 'company-math-symbols-latex)                                                                                      
  (setq company-math-allow-latex-symbols-in-faces t))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :custom
  (dired-use-ls-dired t)
  (dired-dwim-target t)
  (insert-directory-program "/usr/local/bin/gls")
  (dired-listing-switches "-alh --group-directories-first"))

(use-package display-line-numbers
  :custom
  (display-line-numbers 'relative)
  (display-line-numbers-width 4)
  :config
  (set-face-attribute 'line-number-current-line nil :foreground "#ee7621")
  (set-face-attribute 'line-number-current-line nil :background "#f0f0f1"))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy)
  :hydra (dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eglot)

(use-package eglot-jl
  :custom
  (eglot-connect-timeout 600))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package elfeed)

(use-package elpy
  :defer t
  :init
  (elpy-enable)
  :custom
  (elpy-rpc-python-command "~/.pyenv/shims/python")
  (elpy-rpc-virtualenv-path "~/.emacs.d/elpy/rpc-venv"))

(use-package epkg
  :defer t
  :bind
  ("C-h P" . epkg-list-packages)
  ("C-h p" . epkg-describe-package)
  :custom
  (epkg-repository (expand-file-name "var/epkgs/" user-emacs-directory)))

;; inspired by http://www.modernemacs.com/post/custom-eshell/
(use-package eshell
  :defer t
  :custom
  (eshell-prompt-header "\n┌─ ")
  (eshell-prompt-regexp "└─>> ")
  (eshell-prompt-string "└─>> ")
  (eshell-prompt-function 'esh-prompt-func)
  :config
  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat eshell-prompt-header
            "\xf115 "
            (abbreviate-file-name (eshell/pwd))
            "\n"
            eshell-prompt-string)))

;; ess config borrowed from jabranham's init.el
(use-package ess
  ;; ESS (Emacs Speaks Statistics) is a great project that makes Emacs
  ;; speak with R and other statistical languages
  :custom
  (ess-ask-for-ess-directory nil "Don't ask for dir when starting a process")
  (ess-default-style 'RStudio)
  (ess-eldoc-show-on-symbol t "Show eldoc on symbol instead of only inside of parens")
  (ess-eval-visibly 'nowait "Don't hog Emacs")
  (ess-pdf-viewer-pref "emacsclient")
  (ess-use-ido nil)
  (ess-assign-list '(" <- " " -> " " <<- " " ->> "))
  (ess-use-inferior-program-in-buffer-name t)
  :init
  (setq ess-write-to-dribble nil)
  :config
  (setq ess-auto-width 'window)
  (setq ess-execute-in-process-buffer t)
  ;; Save R history in one place rather than making .Rhistory files
  ;; everywhere.
  (setq ess-history-directory (concat user-emacs-directory "var/ESS-history/"))
  ;; Make history folder if needed.
  (mkdir ess-history-directory t)
  (defun my/emacs-q-with-ess ()
    "Start another Emacs with ESS in `load-path'."
    (interactive)
    (start-process-shell-command "emacs" nil (concat "emacs -Q -L " ess-lisp-directory))))

;(use-package ess-julia
;  :defer
;  :mode ("\\.jl\\'" . ess-julia-mode)
;  :config
;  (setq inferior-julia-args "-i --color=yes"))

(use-package ess-r-mode
  :defer
  :bind
  (:map ess-r-mode-map
        ("M-=" . ess-cycle-assign)
        ("M-p" . my/add-pipe)
        ("C-|" . my/ess-eval-pipe-through-line))
  (:map inferior-ess-r-mode-map
        ("M-=" . ess-cycle-assign))
  :config
  (setq inferior-R-args "--no-save")
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . nil)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . nil)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . t)))
  (setq inferior-ess-r-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls . nil)
          (ess-fl-keyword:numbers . nil)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . nil)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (defun my/add-pipe ()
    "Add a pipe operator %>% at the end of the current line.
Don't add one if the end of line already has one.  Ensure one
space to the left and start a newline with indentation."
    (interactive)
    (end-of-line)
    (unless (looking-back "%>%" nil)
      (just-one-space 1)
      (insert "%>%"))
    (newline-and-indent))
  (defun my/advice-ess-cycle-assign ()
    "Call `count-words-region' iff the region is active."
    (when (use-region-p)
      ;; count-words--message echos it to the minibuffer, which is what I want.
      (count-words--message "Region" (region-beginning) (region-end))))
  (advice-add 'ess-cycle-assign :before-until 'my/advice-ess-cycle-assign)
  ;; I sometimes want to evaluate just part of a piped sequence. The
  ;; following lets me do so without needing to insert blank lines or
  ;; something:
  (defun my/ess-beginning-of-pipe-or-end-of-line ()
    "Find point position of end of line or beginning of pipe %>%."
    (if (search-forward "%>%" (line-end-position) t)
        (let ((pos (progn
                     (beginning-of-line)
                     (search-forward "%>%" (line-end-position))
                     (backward-char 3)
                     (point))))
          (goto-char pos))
      (end-of-line)))

  (defun my/ess-eval-pipe-through-line (vis)
    "Like `ess-eval-paragraph' but only evaluates up to the pipe on this line.

If no pipe, evaluate paragraph through the end of current line.

Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
    (interactive "P")
    (save-excursion
      (let ((end (progn
                   (my/ess-beginning-of-pipe-or-end-of-line)
                   (point)))
            (beg (progn (backward-paragraph)
                        (ess-skip-blanks-forward 'multiline)
                        (point))))
        (ess-eval-region beg end vis)))))

(use-package evil
  :custom
  (evil-mode-line-format nil)
  (evil-insert-state-message nil)
  :config (evil-mode 1))

(use-package evil-ledger
  :after ledger-mode
  :custom
  (evil-ledger-sort-key "S")
  :hook ledger-mode)

(use-package evil-lion
  :config
  (evil-lion-mode))

(use-package evil-magit)

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac))
  :config (exec-path-from-shell-initialize))

(use-package eyebrowse
  :config (eyebrowse-mode t))

(use-package faces
  :config
  (set-face-attribute 'default nil :family "Hack")
  (load-theme 'tsdh-light)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#e3e3e5")))

(use-package files
  :config
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))))

(use-package flyspell
  :hook
  (org-mode . flyspell-mode))

(use-package gams-mode
  :custom
  (gams-system-directory "/Applications/GAMS29.1/")
  (gams-docs-directory "/Applications/GAMS29.1/Resources/docs/"))

(use-package geiser)

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package ivy
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))

(use-package julia-mode)

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode))

(use-package ledger-mode
  :mode "\\.ldg\\'"
  :init
  (unbind-key  "C-c C-f" ledger-mode-map)
  :custom
  (ledger-reports
   (quote
    (("Assets" "/usr/local/bin/hledger -f all.ldg balance Assets -V --real")
     ("TotalAssets" "/usr/local/bin/hledger -f all.ldg balance Assets")
     ("BudgetableAssets" "/usr/local/bin/hledger -f all.ldg balance --limit 'account=~/Assets:(Cash|Checking|Gift Cards)/' -V")
     ("Budget" "/usr/local/bin/hledger -f ledger-budget.ldg balance ^Budget --empty --no-total")
     ("CheckingBalances" "/usr/local/bin/hledger -f all.ldg balance Checking Savings --real")
     ("CheckingBalancesCleared" "/usr/local/bin/hledger -f all.ldg balance Checking Savings --cleared --real")
     ("CreditCardBalances" "/usr/local/bin/hledger -f all.ldg balance Liabilities --real")
     ("CreditCardBalancesCleared" "/usr/local/bin/hledger -f all.ldg balance Liabilities --cleared --real")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
  :config
  (defun ledger-occur-uncleared ()
    (interactive)
    (apply 'ledger-occur '("[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9] [^*]")))
; :bind
; (:map ledger-mode-map
;       ("C-c C-f C-u" . ledger-occur-uncleared)
;       ("C-c C-f C-f" . ledger-occur))
  )

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package matlab)

(use-package minions
  :custom (minions-mode-line-lighter "&")
  :config
  (minions-mode))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)

  (defvar rtw/moody-vc-mode
    '(:eval (moody-tab (replace-regexp-in-string "Git[:-]" "\xf841 " (substring vc-mode 1)) nil 'up)))
  (put 'rtw/moody-vc-mode 'risky-local-variable t)
  (make-variable-buffer-local 'rtw/moody-vc-mode)
  (moody-replace-element '(vc-mode vc-mode) '(vc-mode rtw/moody-vc-mode))

  (defvar rtw/moody-evil-state
    '(:eval (when (bound-and-true-p evil-local-mode)
              (let ((color
                    (cond ((evil-normal-state-p)   "#cce6f1")
                          ((evil-emacs-state-p)    "#f1a2a4")
                          ((evil-insert-state-p)   "#cae2ca")
                          ((evil-motion-state-p)   "#e0c180")
                          ((evil-visual-state-p)   "#edd3ec")
                          ((evil-replace-state-p)  "#cce6f1")
                          ((evil-operator-state-p) "#cce6f1"))))
                (moody-wrap-bookend-with-bg-color
                 (upcase (symbol-name evil-state))
                 nil 'up 'tab nil nil color)))))
  (put 'rtw/moody-evil-state 'risky-local-variable t)
  (make-variable-buffer-local 'rtw/moody-evil-state)
  (moody-replace-element 'mode-line-front-space 'rtw/moody-evil-state))

(use-package notmuch
  :custom
  (notmuch-search-oldest-first nil)
  :config
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode nil))))

(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp)
     (R . t)))
  :custom
  (org-agenda-files '("~/org/courses.org" "~/org/ta.org"))
  (org-confirm-babel-evaluate nil)
  (org-M-RET-may-split-line nil)
  (org-agenda-timegrid-use-ampm t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs))))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○")))

(use-package org-drill)

(use-package org-journal
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-enable-encryption t))

(use-package org-noter)

(use-package org-ref
  :custom
  (org-ref-default-bibliography '("~/Sync/bibliography/references.bib"))
  (org-ref-pdf-directory "~/Sync/bibliography/bibtex-pdfs/")
  (org-ref-completion-library 'org-ref-ivy-cite))

(use-package paren
  :config (show-paren-mode))

(use-package pdf-tools
   :magic ("%PDF" . pdf-view-mode)
   :custom
   (pdf-info-epdfinfo-program (expand-file-name "server/epdfinfo" (borg-worktree "pdf-tools")))
   (pdf-sync-forward-display-pdf-key "<C-return>" "Use C-RET in latex mode to jump to location in pdf file")
   (pdf-view-display-size 'fit-page "Show full pages by default instead of fitting page width.")
   (TeX-view-program-selection '((output-pdf "pdf-tools")) "Use pdf-tools to display pdfs from latex runs.")
   (TeX-view-program-list '(("pdf-tools" ("TeX-pdf-tools-sync-view") nil)))
   :config
   (pdf-tools-install))

(use-package popup)

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package pyenv-mode
  :hook
  (python-mode . pyenv-mode))

;(use-package pyenv-mode-auto
;  :hook
;  (python-mode . pyenv-mode-auto))

(use-package python
  :custom
  (python-flymake-command '("flake8", "-"))
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package rg
  :config (rg-enable-menu))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package shell-pop
  :custom
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))))
  (shell-pop-full-span t)
  (shell-pop-universal-key "s-SPC")
  (shell-pop-window-position "bottom"))

(use-package simple
  :config (column-number-mode)
  :hook
  (org-mode . auto-fill-mode))

(use-package slime
  :custom
  (inferior-lisp-program "/usr/local/bin/clisp")
  (slime-contribs '(slime-fancy)))

;(use-package spaceline
;  :config (spaceline-emacs-theme))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package which-key
  :custom
  (which-key-allow-evil-operators 1)
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode)
  (which-key-show-major-mode))

(use-package vterm)

(use-package yaml-mode)

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

(defun use-fancy-splash-screens-p ()
  "Return t if fancy splash screens should be used."
  (when (and (display-graphic-p)
             (or (and (display-color-p)
          (image-type-available-p 'xpm))
                 (image-type-available-p 'pbm)))
    (let ((frame (fancy-splash-frame)))
      (when frame
  (let* ((img (create-image (fancy-splash-image-file)))
         (image-height (and img (cdr (image-size img nil frame))))
         ;; We test frame-height so that, if the frame is split
         ;; by displaying a warning, that doesn't cause the normal
         ;; splash screen to be used.
         (frame-height (1- (frame-height frame))))
   ;; The original value added to the `image-height' for the test was 19; however,
   ;; that causes the test to fail on X11 by about 1.5 -- so use 17 instead.
    (> frame-height (+ image-height 17)))))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
