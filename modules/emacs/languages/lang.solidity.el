;;; lang.nix.el --- Nix language setup -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package solidity-mode
  :straight t
  :mode "\\.sol\\'"
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
                     '((company-solidity company-capf company-dabbrev-code))))))
  ;; LSP config
  ;; https://github.com/hyperledger-labs/solang-vscode
  ;; cd solang-server && cargo build --release
  (when (and
         (functionp 'lsp)
         (executable-find "nomicfoundation-solidity-language-server"))
    (add-to-list 'lsp-language-id-configuration '(solidity-mode . "solidity"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () (executable-find "nomicfoundation-solidity-language-server")))
      :server-id 'solidity-ls
      :language-id 'solidity
      :major-modes '(solidity-mode)
      :priority -1))
    (add-to-list 'company-backends '(company-lsp))))


;; Solidity mode setup
;;(use-package solidity-mode
;;  :mode "\\.sol\\'"
;;  :config
;;  (setq solidity-comment-style 'slash)
;;  (add-hook 'solidity-mode-hook
;;            (lambda ()
;;              ;; Company mode setup for Solidity
;;              (when (functionp 'company-mode)
;;                (use-package company-solidity
;;                  :config
;;                  (setq-local company-backends '((company-solidity company-capf company-dabbrev-code))))
;;                (company-mode 1))
;;
;;              ;; Flycheck setup for Solidity
;;              (use-package solidity-flycheck
;;                :config
;;                (setq solidity-flycheck-solc-checker-active t
;;                      flycheck-solidity-solc-addstd-contracts t)
;;                (flycheck-mode 1))
;;
;;              ;; LSP setup for Solidity
;;              (when (and (functionp 'lsp)
;;                         (executable-find "nomicfoundation-solidity-language-server"))
;;                (lsp-deferred)))))
;;

(provide 'lang.solidity)
;;; lang.solidity.el ends here
