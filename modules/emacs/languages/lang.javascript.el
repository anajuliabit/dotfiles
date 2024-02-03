;;; lang.javascript.el --- Javascript setup -*- lexical-binding: t; -*-

;;; Commentary:
;;; Relies on rjsx mode + tooling via prettier.js and eslint via flycheck

;;; Code:
(use-package json-mode
  :straight t
  :mode "\\.json\\'")

(defvar flycheck-javascript-eslint-executable)
(defun custom/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global.
src: http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable"
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (if (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)
      (setq-local flycheck-javascript-eslint-executable
                  "~/.nvm/versions/node/v20.2.0/bin/eslint"))))

(provide 'lang.javascript)
;;; lang.javascript.el ends here
