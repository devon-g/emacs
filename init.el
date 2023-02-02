;; Move emacs auto custom config to its own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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

;; Gruvbox theme from doom emacs
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

;; Vertical completion for most menus
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))
;; Annotations for vertico results
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;; Fuzzy completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Perform actions based on what is near point
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim)))

;; Search and navigation commands
;; TODO: Further configure
(use-package consult
  :ensure t
  :bind (([remap Info-search] . consult-info)
	 ;; C-x bindings (ctl-x-map
	 ("C-x b" . consult-buffer))
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))
;; Embark features for consult
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Project management
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode 1))
;; Consult projectile features
(use-package consult-projectile
  :ensure t)

;; Show what keybinds do while i'm still learning
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1))

;; Git integration
(use-package magit
  :ensure t)


;; Mail client
;; Uses msmtp to send mail
;; Uses mbsync to sync mail
(use-package notmuch
  :config
  (setq send-mail-function 'sendmail-send-it)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header))
