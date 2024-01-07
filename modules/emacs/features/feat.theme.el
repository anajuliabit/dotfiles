;;; feat.themeing.el --- Themeing and tools related to themes -*- lexical-binding: t; -*-

;;; Commentary:
;;; Themes and theme related tooling

;;; Code:
(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
)

;; Font settings
(use-package alfontzo
  :straight (alfontzo :type git
		                  :host github
		                  :repo "guidoschmidt/alfontzo")
  :config
  (alfontzo-init))

;; Rainbow delimiters, color highlight parenthesis
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Color coded hex/rgb/hls values
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Use beautiful icons
(use-package all-the-icons-ivy
  :straight t)

;; Use beautiful icons also in ivy
(use-package all-the-icons-ivy-rich
  :straight t
  :config
  (all-the-icons-ivy-rich-mode t))

;; highliht indentation
(use-package highlight-indent-guides
  :straight t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  :hook (prog-mode . highlight-indent-guides-mode))

;; Set line spacing
(setq-default line-spacing 2)

;; highlight buffers
(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

(provide 'feat.theme)
;;; feat.theme.el ends here
