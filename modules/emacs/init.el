(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
;;(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Mono" :height 140)

;; Display numbers
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
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :config (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t)))

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

;;; package --- lsp mode settings
(use-package lsp-mode
  :defer t
  :ensure t
  :hook ((nix-mode rustic-mode) . lsp)
  :commands (lsp lsp-deferred lsp-register-client)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-lens-enable t)
  )

(use-package lsp-ui
  :ensure t
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

(use-package solidity-mode
  :ensure t
  :mode ("\\.sol\\'" . solidity-mode)
  :config
  (setq solidity-comment-style 'slash)
)

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


;; solidity LSP
;;(after! eglot
;;  (add-to-list 'eglot-server-programs
;;               '(solidity-mode . ("nomicfoundation-solidity-language-server" "--stdio"))))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


