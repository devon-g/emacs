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

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Reduce load time
(eval-when-compile (require 'use-package))

;; Organize your life
(use-package org
  :hook ((org-mode . org-indent-mode)
	 (org-mode . visual-line-mode)))

;; Gruvbox theme from doom emacs
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-gruvbox t))

;; Minibuffer completion
(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :custom (ivy-use-virtual-buffers t))

;; Integrate ivy with common commands
(use-package counsel
  :ensure t
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
  :ensure t
  :bind (("C-s" . swiper)))

;; Better help results
(use-package helpful
  :ensure t
  :bind (("C-h k" . #'helpful-key)
	 ("C-c C-d" . #'helpful-at-point)
	 ("C-h F" . #'helpful-function)
	 ("C-h C" . #'helpful-command))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; Project management
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config (projectile-mode 1))

;; Counsel support for projectile
(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode 1))

;; Show what keybinds do while i'm still learning
(use-package which-key
  :ensure t
  :init (which-key-mode 1))

;; Git integration
(use-package magit
  :ensure t)

;; Extension for magit
(use-package forge
  :ensure t)

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

;; LSP Support
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . #'lsp-enable-which-key-integration))

;; Snippets
(use-package yasnippet-snippets
  :ensure t)

;; Snippet support
(use-package yasnippet
  :ensure t
  :config (yas-reload-all)
  :hook (lsp-mode . yas-minor-mode))

;; Auto completion for lsp-mode
(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; UI for lsp-mode
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

;; Syntax checking
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

;; Configure rust stuff
(use-package rustic
  :ensure t
  :config (setq rustic-format-on-save t))

;; Configure c++ stuff
(use-package cc-mode
  :hook
  (c-mode . lsp)
  (c++-mode . lsp))
