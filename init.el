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
  (setq mac-option-modifier 'super)
  (global-unset-key (kbd "<wheel-left>"))
  (global-unset-key (kbd "<wheel-right>")))

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

(progn ;
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (require 'pragmatapro-prettify-symbols-v0.829))

(defun brew-prefix (command)
  "Prepend appropriate homebrew prefix to COMMAND."
  (concat (substring (shell-command-to-string "brew --prefix") 0 -1) command))

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

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package beacon
  :bind ("<s-escape>" . beacon-blink)
  :custom
  (beacon-color "#fb6859")
  :config (beacon-mode -1))

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
  (LaTeX-mode . yas-minor-mode)
  (LaTeX-mode . flyspell-mode)
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
  :bind ("M-C" . calendar)
  :custom
  (calendar-week-start-day 1)
  :hook
  (calendar-today-visible . calendar-mark-today))

(use-package calfw
  :config
  (use-package calfw-org))

(use-package cdlatex
  :custom
  (cdlatex-use-dollar-to-ensure-math nil)
  :config
  (setq cdlatex-math-modify-alist
        '((?B "\\mathbb"   nil t nil nil)
          (?F "\\mathfrak" nil t nil nil)
          (?s "\\mathscr"  nil t nil nil))))

(use-package consult)

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode)
  :config
  (setq completion-cycle-threshold 3))

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
  (insert-directory-program (brew-prefix "/bin/gls"))
  (dired-listing-switches "-alh --group-directories-first"))

(use-package dired-sidebar
  :after dired
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-use-custom-font t)
  (dired-sidebar-width 40)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode)))))

(use-package dired-subtree
  :after dired
  :config
  (set-face-attribute 'dired-subtree-depth-1-face nil :background "#dbdbdb")
  (setq dired-subtree-use-backgrounds t)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<S-right>" . dired-subtree-cycle)
              ("<S-up>" . dired-subtree-remove)))

(use-package display-line-numbers
  :custom
  (display-line-numbers 'relative)
  (display-line-numbers-width 4)
  :config
  (set-face-attribute 'line-number-current-line nil :foreground "#ee7621")
  (set-face-attribute 'line-number-current-line nil :background "#f0f0f1"))

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-day t))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eglot
  :after eldoc flymake project
  :init
  (add-hook 'rust-mode-hook 'eglot-ensure)
  :custom
  (eglot-extend-to-xref t)
  :config
  (setq completion-category-overrides '((eglot (styles orderless))))

;; add SQL Language Server support to eglot
;; from https://old.reddit.com/r/emacs/comments/ijbvwv/eglot_sqls_sql_client/
  (defclass eglot-sqls (eglot-lsp-server) ()
    :documentation "SQL's Language Server")
  (add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls" "-config" "~/.config/sqls/config.yaml")))
  (cl-defmethod eglot-execute-command ((server eglot-sqls) (command (eql executeQuery)) arguments)
    "For executeQuery."
    (let* ((beg (eglot--pos-to-lsp-position (if (use-region-p) (region-beginning) (point-min))))
           (end (eglot--pos-to-lsp-position (if (use-region-p) (region-end) (point-max))))
           (res (jsonrpc-request server
                                 :workspace/executeCommand `(:command ,(format "%s" command)
                                                                      :arguments ,arguments
                                                                      :timeout 0.5
                                                                      :range (:start ,beg :end ,end))))
           (buffer (generate-new-buffer "*sqls*")))
      (with-current-buffer buffer
        (eglot--apply-text-edits `[(:range (:start (:line 0 :character 0)
                                                   :end (:line 0 :character 0))
                                           :newText ,res)])
        (org-mode))
      (pop-to-buffer buffer)))
  (cl-defmethod eglot-execute-command ((server eglot-sqls) (_cmd (eql switchDatabase)) arguments)
    "For switchDatabase."
    (let* ((res (jsonrpc-request server :workspace/executeCommand
                                 `(:command "showDatabases" :arguments ,arguments :timeout 0.5)))
           (menu-items (split-string res "\n"))
           (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
           (db (if (listp last-nonmenu-event)
                   (x-popup-menu last-nonmenu-event menu)
                 (completing-read "[eglot] Pick an database: "
                                  menu-items nil t nil nil (car menu-items)))))
      (jsonrpc-request server
                       :workspace/executeCommand `(:command "switchDatabase"
                                                            :arguments [,db]:timeout
                                                            0.5))))
  (cl-defmethod eglot-execute-command ((server eglot-sqls) (_cmd (eql switchConnections)) arguments)
    "For switchConnections"
    (let* ((res (jsonrpc-request server :workspace/executeCommand
                                 `(:command "switchConnections" :arguments ,arguments :timeout 0.5)))
           (menu-items (split-string res "\n"))
           (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
           (db (if (listp last-nonmenu-event)
                   (x-popup-menu last-nonmenu-event menu)
                 (completing-read "[eglot] Pick an connection "
                                  menu-items nil t nil nil (car menu-items)))))
      (jsonrpc-request server :workspace/executeCommand
                       `(:command "switchConnections" :arguments [,db]:timeout 0.5)))))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package electric-operator
  :hook rust-mode
  :config
  (electric-operator-add-rules-for-mode 'rust-mode
                                        (cons "||" nil)))

(use-package elfeed)

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

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init '(compile dired eglot elfeed eshell ibuffer magit notmuch ripgrep selectrum)))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac))
  :config (exec-path-from-shell-initialize))

(use-package faces
  :config
  (set-face-attribute 'default nil :family "PragmataPro Mono" :height 130)
  ;; (load-theme 'tsdh-light)
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

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :hook
  ((emacs-lisp-mode LaTeX-mode python-mode rust-mode) . flymake-mode))

(use-package flyspell
  :hook
  (org-mode . flyspell-mode))

(use-package forge
  :after magit)

(use-package gams-mode
  :custom
  (gams-system-directory "/Applications/GAMS29.1/")
  (gams-docs-directory "/Applications/GAMS29.1/Resources/docs/"))

(use-package general
  :config
  (general-create-definer space :prefix "SPC")
  (general-create-definer space-b :prefix "SPC b")
  (general-create-definer space-o :prefix "SPC o")

  (space
   :states 'normal
;;   "SPC" '(counsel-M-x :wk "M-x")
;;   "f" '(counsel-find-file :wk "find file")
   "g" 'magit-status
   "p" 'projectile-command-map
   "w" 'hydra-window/body
   "z" 'hydra-zoom/body)

  (defun rtw/switch-to-home ()
    (interactive)
    (switch-to-buffer (get-buffer "*GNU Emacs*")))
  (defun rtw/switch-to-scratch ()
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*")))
 (space-b
   :states 'normal
;;   "b" 'ivy-switch-buffer
   "c" 'delete-frame
   "d" 'delete-window
   "f" 'delete-other-windows
   "k" 'kill-buffer
   "h" 'split-window-horizontally
   "l" 'list-buffers
   "n" 'make-frame-command
  ;"h" '(rtw/switch-to-home :wk "home")
   "s" '(rtw/switch-to-scratch :wk "*scratch*")
   "v" 'split-window-vertically)

  (space-o
   :states 'normal
   "a" 'org-agenda
   "c" 'org-capture
   "j" 'org-journal-new-entry))

(use-package go-mode)

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package hideshow
  :config
  (add-hook 'rust-mode-hook #'hs-minor-mode))

(use-package hydra
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

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package julia-mode)

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode))

(use-package ledger-mode
  :mode "\\.ldg\\'"
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
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode))


(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-git-executable (brew-prefix "/bin/git"))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package markdown-mode
  :init (setq markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

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

(use-package orderless
  :config
  (setq completion-styles '(orderless)))

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
     (R . t)
     (sql . t)))
  (setq org-agenda-custom-commands
        '(("c" "Custom Agenda View"
           ((org-ql-block '(and (priority "A")
                                (not (todo "DONE")))
                          ((org-ql-block-header "Today:")))
             (agenda "")
             (org-ql-block '(and (not (priority "A"))
                                     (todo))
                           ((org-ql-block-header "Other TODOs")))))))
  :custom
  (org-agenda-files '("~/org/courses.org" "~/org/ta.org" "~/org/research.org"))
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

(use-package org-journal
  :custom
  (org-journal-dir "~/Documents/journal/")
  (org-journal-enable-encryption t))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode))

(use-package org-ref
  :defer t
  :custom
  (org-ref-default-bibliography '("~/Sync/bibliography/references.bib"))
  (org-ref-pdf-directory "~/Sync/bibliography/bibtex-pdfs/"))

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

(use-package prescient
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode +1))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'prog-mode-hook #'indent-spaces-mode)
  (add-hook 'prog-mode-hook #'prettify-hook))

(use-package project)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package python
  :custom
  (python-flymake-command '("flake8", "-"))
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package rg
  :custom
  (rg-executable (brew-prefix "/bin/rg"))
  :config (rg-enable-menu))

(use-package rust-mode
  :custom
  (rust-format-on-save nil)
  (rust-indent-method-chain t)
  (rust-indent-where-clause t))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package selectrum
  :config
  (selectrum-mode +1)
  (setq selectrum-prescient-enable-filtering nil))

(use-package shell-pop
  :custom
  (shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm))))
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

(use-package sql
  :defer t
  :config
  (sql-set-product "postgres"))

(use-package sqlformat
  :after sql
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("--comma-start" "-s4" "-C" "-w120"))
  :config
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat)
  (add-hook 'sql-mode-hook #'sqlformat-on-save-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package tree-sitter
  :after tsc
  :load-path "lib/tree-sitter/lisp"
  :hook
  (rust-mode . tree-sitter-mode))

(use-package tree-sitter-debug
  :after tsc tree-sitter
  :load-path "lib/tree-sitter/lisp")

(use-package tree-sitter-hl
  :after tsc tree-sitter
  :load-path "lib/tree-sitter/lisp"
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tsc tree-sitter
  :config
  (tree-sitter-require 'rust))

(use-package tree-sitter-query
  :after tsc tree-sitter
  :load-path "lib/tree-sitter/lisp")

(use-package tsc
  :load-path "lib/tree-sitter/core")

(use-package which-key
  :custom
  (which-key-allow-evil-operators 1)
  (which-key-idle-delay 0.1)
  :config
  (which-key-mode)
  (which-key-show-major-mode))

(use-package visual-fill-column)

(use-package vterm
  :custom
  (vterm-shell (brew-prefix "/bin/fish"))
  (vterm-ignore-blink-cursor t))

(use-package yaml-mode)

(use-package yasnippet
  :bind (:map global-map
              ("M-i" . nil))
  :bind (:map yas-minor-mode-map
              ("M-i" . yas-expand)
              ("M-I" . yas-insert-snippet))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/lib/yasnippet-snippets/snippets/rust-mode/"))
  (add-hook 'rust-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package zig-mode)

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
