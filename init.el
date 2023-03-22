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

;; Gruvbox theme from doom emacs
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-gruvbox t))

;; Nice organization stuff
(use-package org
  :straight (:type built-in)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)))

;; Better minibuffer stuff
(use-package vertico
  :straight t
  :init (vertico-mode 1))

;; Auto completion
(use-package corfu
  :straight t
  :load-path "straight/repos/corfu/extensions/"
  :custom
  (corfu-auto t)
  (corfu-popupinfo-delay t)
  :init (global-corfu-mode 1))

;; Rich annotations for vertico
(use-package marginalia
  :straight t
  :init (marginalia-mode 1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-override '((file (styles basic partial-completion)))))

;; Better help results
(use-package helpful
  :straight t
  :bind (("C-h f" . #'helpful-callable)
	 ("C-h v" . #'helpful-variable)
	 ("C-h k" . #'helpful-key)
	 ("C-c C-d" . #'helpful-at-point)
	 ("C-h F" . #'helpful-function)
	 ("C-h C" . #'helpful-command)))

;; Project management
(use-package projectile
  :straight t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode 1))

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

;; LSP Support
(use-package lsp-mode
  :straight t
  :commands lsp
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
		    :major-modes '(c++-mode)
		    :remote? t
		    :server-id 'ros2vm))
  :hook
  (python-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration))

;; Snippet support
(use-package yasnippet-snippets
  :straight t)
(use-package yasnippet
  :straight t
  :config (yas-reload-all)
  :hook (lsp-mode . yas-minor-mode))

;; UI for lsp-mode
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

;; Syntax checking
(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

;; Syntax highlighting
(use-package tree-sitter-langs
  :straight t)
(use-package tree-sitter
  :straight t
  :hook ((python-mode) . tree-sitter-hl-mode)
  :config (global-tree-sitter-mode 1))

;; Configure rust stuff
(use-package rustic
  :straight t
  :config (setq rustic-format-on-save t))

;; Nginx stuff
(use-package nginx-mode
  :straight t)

;; Latex stuff
(use-package auctex
  :straight t
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-view-program-selection '((output-pdf "PDF Tools")) TeX-source-correlate-start-server t))
(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . auto-revert-mode)
  :config
  (pdf-loader-install))

(setq auto-revert-interval 0.1)

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
