(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
  :bind
  ("M-j" . 'avy-goto-char-timer))

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
