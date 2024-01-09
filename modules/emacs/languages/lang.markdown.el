;;; lang.markdown.el --- Markdown setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package markdown-mode
  :straight t
  :hook
  ;; uncomment and choose your prefared lsp packend server.
  ;; lsp-mode.
  ;;(markdown-mode . lsp-deferred)
  ;; eglot mode.
  (markdown-mode . eglot-ensure)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.mdx\\'" . markdown-mode))       ;; Markdown for the component era.
  ("\\.markdown\\'" . markdown-mode)
  ;;:commands (markdown-mode gfm-mode)
  :init
  ;; tools command to preview.
  ;; ;; for simple things multimarkdown
  ;; (setq markdown-command "multimarkdown")
  ;; pandoc 
  ;; (setq markdown-command "pandoc -t html5")
  :config
  ;; ;; load marksman directory to be placed of.
  ;; (add-to-list 'exec-path "~/.local/bin")
  ;; uncomment if you are using lsp-mode.
  ;;(require 'lsp-marksman)
  ;; configuration for eglot using marksman
  ;;(with-eval-after-load 'eglot
  ;;  (add-to-list 'eglot-server-programs
  ;;               '(markdown-mode . ("marksman" "--stdio"))))
  )

;;; lang.markdown.el ends here
