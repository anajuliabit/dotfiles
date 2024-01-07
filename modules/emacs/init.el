(setq inhibit-startup-message t)
(defvar chidori-cache-dir (expand-file-name "cache/" user-emacs-directory))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar


;; Fullscreen by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable backup files
(setq make-backup-files nil)

(set-face-attribute 'default nil :font "Fira Mono" :height 140)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
	        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map

	 ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-selectable-prompt t)
  )


(use-package counsel
  :ensure t
  :init (counsel-mode 1)
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r". counsel-minibuffer-history))
  )

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :hook (after-init . all-the-icons-completion-mode)
  :if (display-graphic-p)
  )

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15))
  )

;;(use-package rainbow-delimiters
;;  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; completions
(use-package vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map)
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :config
  (vertico-mode))

;; completions annotations
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Solidity mode setup
(use-package solidity-mode
  :mode "\\.sol\\'"
  :config
  (setq solidity-comment-style 'slash)
  (add-hook 'solidity-mode-hook
            (lambda ()
              ;; Company mode setup for Solidity
              (when (functionp 'company-mode)
                (use-package company-solidity
                  :config
                  (setq-local company-backends '((company-solidity company-capf company-dabbrev-code))))
                (company-mode 1))

              ;; Flycheck setup for Solidity
              (use-package solidity-flycheck
                :config
                (setq solidity-flycheck-solc-checker-active t
                      flycheck-solidity-solc-addstd-contracts t)
                (flycheck-mode 1))

              ;; LSP setup for Solidity
              (when (and (functionp 'lsp)
                         (executable-find "nomicfoundation-solidity-language-server"))
                (lsp-deferred)))))

(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
  :defer t
  :straight t
  :hook ((nix-mode . lsp)
         (rustic-mode . lsp)
         (solidity-mode . lsp)
         (lisp-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred))
  :commands (lsp lsp-deferred lsp-register-client)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-lens-enable t)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("nomicfoundation-solidity-language-server" "--stdio"))
    :major-modes '(solidity-mode)
    :priority -1
    :server-id 'solidity-ls))
  )

(use-package lsp-ui
  :after lsp-mode
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-sideline-delay 0.5
        lsp-ui-sideline-enable nil))

                                        ;(use-package dap-mode)
                                        ;(require 'dap-gdb-lldb)
                                        ;(require 'dap-codelldb)
                                        ;(use-package realgud)
                                        ;(use-package realgud-lldb)

(provide 'pkg-solidity-mode)
;;; package --- Company (autocomplete) settings
(use-package company
  :ensure t
  :after (lsp-mode)
  :commands (company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection)
         :map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :config
  (global-company-mode 1)
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0))

(use-package flycheck
  :ensure t
  :after lsp-mode
  :config
  (global-flycheck-mode)
  (setq flycheck-display-errors-function nil))


;; solidity LSP
;;(after! eglot
;;  (add-to-list 'eglot-server-programs
;;               '(solidity-mode . ("nomicfoundation-solidity-language-server" "--stdio"))))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package forge
  :straight t
  :after magit)

(use-package projectile
  :ensure t
  :init

  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  ;;(add-to-list 'lsp-language-id-configuration '(solidity-mode . "solidity"))
  ;;(add-hook 'prog-mode-hook 'copilot-mode)
  :hook (prog-mode . copilot-mode))

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "/Users/anajulia/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/notes/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

;; Define key for open dired with C-x C-d (default is C-x d)
(global-set-key (kbd "C-x C-d") 'dired)


(use-package vterm
   :straight t)

(use-package nix-mode :mode "\\.nix\\'")
