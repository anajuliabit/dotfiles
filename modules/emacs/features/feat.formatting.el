;;; feat.formatting.el --- Code formatting tools -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;;(use-package format-all
;;  :straight t)
;;
;;(use-package prettier-js
;;  :straight t
;;  :config
;;  (setq prettier-js-args '("--trailing-comma" "all"
;;                           "--bracket-spacing" "false"
;;                           "--single-quote" "false"
;;                           "--print-width" "80"
;;                           ))
;;  :hook
;;  ((solidity-mode . prettier-js-mode)
;;   (js2-mode  . prettier-js-mode)))
;;(add-hook 'before-save-hook 'prettier-js)
(remove-hook 'before-save-hook 'prettier-js)
(setq before-save-hook nil)




(provide 'feat.formatting)
;;; feat.formatting.el ends here
