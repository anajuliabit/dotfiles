;;; init.el --- Main entry for Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
;; set nix-dotfiles path
(defvar config-path (expand-file-name "~/nix-dotfiles/modules/emacs")
  "Path to nix-dotfiles.")
(load (expand-file-name "core/core.configuration.el" config-path))
(load (expand-file-name "core/core.straight.el" config-path))
(load (expand-file-name "features/feat.completion.el" config-path))
(load (expand-file-name "features/feat.editor.el" config-path))
(load (expand-file-name "features/feat.evil.el" config-path))
(load (expand-file-name "languages/lang.typescript.el" config-path))

(dolist (mode '(org-mode-hook
                term-mode-hook
	        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq use-package-always-ensure t)

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

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'pkg-solidity-mode)

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

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Define key for open dired with C-x C-d (default is C-x d)
(global-set-key (kbd "C-x C-d") 'dired)


(use-package vterm
   :straight t)

(use-package nix-mode :mode "\\.nix\\'")

(provide 'init.el)
;;; init.el ends here
