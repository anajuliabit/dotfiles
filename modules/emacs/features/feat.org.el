;;; feat.org.el --- Org mode setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Org mode configuration and setup

;;; Code:

;;(defun my/org-mode-setup ()
;;  (org-bullets-mode 1)
;;  (org-indent-mode)
;;  (variable-pitch-mode 1)
;;  (auto-fill-mode 0)
;;  (visual-line-mode 1)
;;  (setq evil-auto-indent nil)
;;  (diminish org-indent-mode)

;;; Hide the emphasis markup
;;; (e.g. /.../ for italics, *...* for bold, etc.)
(setq org-hide-emphasis-markers t
      org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)")))
      )

  ;; Prettify UI
(use-package org-modern
  :straight t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
         (org-modern-mode . (lambda ()
                              "Adapt `org-modern-mode'."
                              ;; Disable Prettify Symbols mode
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1)))))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory (file-truename "/Users/anajulia/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/notes/main")
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        )
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight t
  :bind ("C-c n u" . org-roam-ui-mode)
  :init (when (featurep 'xwidget-internal)
          (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))


(provide 'feat.org)
;;; feat.org.el ends here
