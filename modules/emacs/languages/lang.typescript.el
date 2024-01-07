;;; lang.typescript.el --- Typescript configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration of Typescript and tooling

;;; Code:
;;  tree-sitter indentation minor mode for Emacs
(use-package tsi
  :straight (tsi
            :type git
            :host github
            :repo "orzechowskid/tsi.el"))

(use-package typescript-mode
  :straight t
  :config
  (setq-default typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'emmet/ts-mode-hook)
  :mode (("\\.ts\\'"  . typescript-mode))
  )

(use-package tide
  :straight t
  :after
  (typescript-mode company flycheck)
  :config
  (setq company-tooltip-align-annotations t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (defun typescript-company-mode-hook ()
    (setq-local company-backends
                '((company-tide :with company-dabbrev company-yasnippet))))
  (add-hook 'typescript-mode #'typescript-company-mode-hook)
    :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . flycheck-mode)
   (typescript-mode . eldoc-mode)
   (typescript-mode . company-mode)
   (typescript-mode . tide-hl-identifier-mode)))

(provide 'lang.typescript)
;;; lang.typescript.el ends here
