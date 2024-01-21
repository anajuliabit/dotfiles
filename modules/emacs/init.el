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
(load (expand-file-name "features/feat.functions.el" config-path))
(load (expand-file-name "features/feat.git.el" config-path))
(load (expand-file-name "features/feat.hydra.el" config-path))
(load (expand-file-name "features/feat.org.el" config-path))
(load (expand-file-name "features/feat.copilot.el" config-path))
(load (expand-file-name "features/feat.syntaxchecking.el" config-path))
(load (expand-file-name "features/feat.modeline.el" config-path))
(load (expand-file-name "features/feat.formatting.el" config-path))
(load (expand-file-name "features/feat.theme.el" config-path))
(load (expand-file-name "languages/lang.typescript.el" config-path))
(load (expand-file-name "languages/lang.nix.el" config-path))
(load (expand-file-name "languages/lang.solidity.el" config-path))
(load (expand-file-name "languages/lang.javascript.el" config-path))
(load (expand-file-name "languages/lang.markdown.el" config-path))

;;(dolist (mode '(org-mode-hook
;;                term-mode-hook
;;	        eshell-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;(setq use-package-always-ensure t)

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

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; solidity LSP
;;(after! eglot
;;  (add-to-list 'eglot-server-programs
;;               '(solidity-mode . ("nomicfoundation-solidity-language-server" "--stdio"))))

(use-package vterm
   :straight t)

(provide 'init.el)
;;; init.el ends here
