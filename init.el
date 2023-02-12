;; Move emacs auto custom config to its own file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Keep backups and autosaves in a sane location
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Disable ugly things
(setq inhibit-startup-message t)
(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(menu-bar-mode -1)    ; Disable the menu bar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 0)   ; Remove fringe bars on sides
(setq visible-bell t)

;; Set font and size because this shit too small by default
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 175)

;; Enable column info in status line
(column-number-mode)
;; Enable line numbers when in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Install straight.el if it isn't already
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Get use package ready
(straight-use-package 'use-package)

;; Better minibuffer stuff
(use-package vertico
  :straight t
  :init (vertico-mode))

;; Auto completion
(use-package corfu
  :straight t
  :custom (corfu-auto t)
  :init (global-corfu-mode))

;; Rich annotations for vertico
(use-package marginalia
  :straight t
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

;; Better auto completion ordering
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Gruvbox theme from doom emacs
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-gruvbox t))

;; Better help results
(use-package helpful
  :straight t
  :bind (("C-h f" . #'helpful-callable)
	 ("C-h v" . #'helpful-variable)
	 ("C-h k" . #'helpful-key)
	 ("C-c C-d" . #'helpful-at-point)
	 ("C-h F" . #'helpful-function)
	 ("C-h C" . #'helpful-command)))

;; Show what keybinds do while i'm still learning
(use-package which-key
  :straight t
  :init (which-key-mode 1))

;; Git integration
(use-package magit
  :straight t)

;; LSP
(use-package eglot
  :straight t)

;; Snippets
(use-package yasnippet-snippets
  :straight t)
(use-package yasnippet
  :straight t
  :config (yas-global-mode))

;; Syntax highlighting
(use-package tree-sitter-langs
  :straight t)
(use-package tree-sitter
  :straight t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config (global-tree-sitter-mode))

;; Rust stuff
(use-package rustic
  :straight t
  :config (setq rustic-format-on-save t)
  :custom (rustic-lsp-client 'eglot))

;; Nginx stuff
(use-package nginx-mode
  :straight t)

;; Mail client
;; Uses msmtp to send mail
;; Uses mbsync to sync mail
(use-package notmuch
  :config
  (setq send-mail-function 'sendmail-send-it)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  :custom
  (setq message-sendmail-envelope-from 'header))
