;;; lang.nix.el --- Nix language setup -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package solidity-mode
  :straight t
  :mode (("\\.sol\\'" . solidity-mode))
  :bind-keymap
  ("C-c C-g" . solidity-mode-map)
  :config
  (setq solidity-comment-style 'slash)
  (require 'solidity-flycheck)
  (setq solidity-flycheck-solc-checker-active t
        flycheck-solidity-solc-addstd-contracts t)
  (when (functionp 'company-mode)
    (require 'company-solidity)
    (add-hook 'solidity-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-solidity company-capf company-dabbrev-code)))
            (setq fill-column 80)
            (auto-fill-mode 1))))
  (add-to-list 'company-backends '(company-lsp)))

(provide 'lang.solidity)
;;; lang.solidity.el ends here
