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
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 150)

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

;; Organize your life
(use-package org
  :hook ((org-mode . org-indent-mode)
	 (org-mode . visual-line-mode)))

;; Gruvbox theme from doom emacs
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-gruvbox t))

;; Minibuffer completion
(use-package ivy
  :straight t
  :init (ivy-mode 1)
  :custom (ivy-use-virtual-buffers t))

;; Integrate ivy with common commands
(use-package counsel
  :straight t
  :bind (("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("M-y" . counsel-yank-pop)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x r b" . counsel-bookmark)
	 ("C-h a" . counsel-apropos-command)
	 ("C-h b" . counsel-descbinds)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
	 ("C-h o" . counsel-describe-symbol)
	 ("C-h l" . counsel-find-library)
	 ("C-h S" . counsel-info-lookup-symbol)
	 ("C-h u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate)
	 ("C-S-o" . counsel-rhythmbox)))

;; Better searching
(use-package swiper
  :straight t
  :bind (("C-s" . swiper)))

;; Better help results
(use-package helpful
  :straight t
  :bind (("C-h k" . #'helpful-key)
	 ("C-c C-d" . #'helpful-at-point)
	 ("C-h F" . #'helpful-function)
	 ("C-h C" . #'helpful-command))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; Project management
(use-package projectile
  :straight t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config (projectile-mode 1))

;; Counsel support for projectile
(use-package counsel-projectile
  :straight t
  :config (counsel-projectile-mode 1))

;; Show what keybinds do while i'm still learning
(use-package which-key
  :straight t
  :init (which-key-mode 1))

;; Git integration
(use-package magit
  :straight t)

;; Extension for magit
(use-package forge
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

;; Popup windows for lsp-bridge
(use-package posframe
  :straight t)

;; lsp-bridge dependency
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Snippet support
(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

;; Snippets
(use-package yasnippet-snippets
  :straight t)

;; LSP
(add-to-list 'load-path "~/.config/emacs/straight/repos/lsp-bridge")
(use-package lsp-bridge
  :config (global-lsp-bridge-mode)
  :custom
  (lsp-bridge-c-lsp-server "clangd")
  (lsp-bridge-python-lsp-server "pylsp")
  (lsp-bridge-python-ruff-lsp-server "pylsp_ruff"))

;; lsp-bridge terminal support
(unless (display-graphic-p)
  (add-to-list 'load-path "~/.config/emacs/straight/repos/acm-terminal")
  (with-eval-after-load 'acm
    (require 'acm-terminal)))

;; Syntax checking
(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

;; Syntax highlighting
(use-package tree-sitter
  :straight t
  :config (add-to-list 'tree-sitter-major-mode-language-alist '(python-mode . python))
  :hook ((python-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

;; Configure rust stuff
(use-package rustic
  :straight t
  :config (setq rustic-format-on-save t))

;; Nginx stuff
(use-package nginx-mode
  :straight t)
