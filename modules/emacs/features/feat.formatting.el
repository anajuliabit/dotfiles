;;; feat.formatting.el --- Code formatting tools -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package format-all
  :straight t)

(use-package prettier-js
  :straight t
  :config
  (setq prettier-js-args '("--trailing-comma" "all"
                           "--bracket-spacing" "true"
                           "--single-quote" "true"
                           "--print-width" "80"
                           "--plugin" "/Users/anajulia/.local/share/rtx/installs/node/18.19.0/lib/node_modules/prettier-plugin-solidity"
                           ))
  :hook
  ((solidity-mode . prettier-js-mode)
   (js2-mode  . prettier-js-mode)))

(add-hook 'before-save-hook 'prettier-js)



(provide 'feat.formatting)
;;; feat.formatting.el ends here
