;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ana Julia"
      user-mail-address "anajuliabit@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


(setq solidity-comment-style 'slash)

(use-package! solidity-flycheck  ; included with solidity-mode
  :when (modulep! :checkers syntax)
  :after solidity-mode
  :config
  (set-docsets! 'solidity-mode "Solidity")
  (setq flycheck-solidity-solc-addstd-contracts t)
  (when (funcall flycheck-executable-find solidity-solc-path)
    (add-to-list 'flycheck-checkers 'solidity-checker nil #'eq))
  (when (funcall flycheck-executable-find solidity-solium-path)
    (add-to-list 'flycheck-checkers 'solium-checker nil #'eq)))


(use-package! company-solidity
  :when (modulep! :completion company)
  :after solidity-mode
  :config
  (delq! 'company-solidity company-backends)
  (set-company-backend! 'solidity-mode 'company-solidity))

;; keep the cursor centered to avoid sudden scroll jumps
;; disable in terminal modes
;; http://stackoverflow.com/a/6849467/519736
;; also disable in Info mode, because it breaks going back with the backspace key
(define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'Info-mode 'term-mode 'eshell-mode 'shell-mode 'erc-mode 'vterm-mode)))
      (centered-cursor-mode))))
(my-global-centered-cursor-mode 1)


;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(after! org
  (setq org-directory "~/.personal/"
        org-agenda-start-with-log-mode t
        org-agenda-files '("~/.personal/agenda")
        org-agenda-category-icon-alist
        `(("home" ,(list (all-the-icons-faicon "home" :v-adjust -0.05)) nil nil :ascent center :mask heuristic)
          ("inbox" ,(list (all-the-icons-faicon "inbox" :v-adjust -0.1)) nil nil :ascent center :mask heuristic)
          ("people" ,(list (all-the-icons-material "people" :v-adjust -0.25)) nil nil :ascent center :mask heuristic)
          ("work" ,(list (all-the-icons-material "work" :v-adjust -0.25)) nil nil :ascent center :mask heuristic)
          ("routine" ,(list (all-the-icons-material "repeat" :v-adjust -0.25)) nil nil :ascent center :mask heuristic)
          )
        org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("h" "Home Tasks" tags-todo "@home")
          ("w" "Work Tasks" tags-todo "@work")

          ("E" "Easy Tasks" tags-todo "easy")
          ("C" "Challenging Tasks" tags-todo "challenging")

          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files))))
        org-agenda-dim-blocked-tasks t
        org-agenda-inhibit-startup t
        org-agenda-show-log t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 2
        org-agenda-start-on-weekday 6
        org-agenda-start-with-log-mode t
        org-agenda-sticky nil
        org-agenda-tags-column 90
        org-agenda-time-grid '((daily today require-timed))
        org-agenda-use-tag-inheritance t
        org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS"
        org-default-notes-file "~/.personal/agenda/inbox.org"
        org-ellipsis "  "                ; nerd fonts chevron character
        org-use-property-inheritance t
        org-log-done 'time
        ;;org-hide-emphasis-markers t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-habit-completed-glyph ?✓
        org-habit-graph-column 80
        org-habit-show-habits-only-for-today nil
        org-habit-today-glyph ?‖
        org-track-ordered-property-with-tag t
        org-log-into-drawer t
        org-log-state-notes-into-drawer t
        org-log-repeat 'time
        org-todo-repeat-to-state "TODO"
        +org-capture-todo-file "home.org"
        +org-capture-notes-file "slipbox.org"
        org-archive-location "~/.personal/archives/%s::"
        org-refile-targets
        '(("archive.org" :maxlevel . 1))
        deft-directory "~/.personal"
        deft-recursive t
        org-blank-before-new-entry '((heading . t) (plain-list-item . t))
        org-confirm-babel-evaluate nil
        org-cycle-include-plain-lists 'integrate
        org-export-backends '(ascii beamer html icalendar latex man md org texinfo)
        org-hide-emphasis-markers t
        org-modules '(org-crypt org-habit org-mouse org-protocol org-tempo)
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files :maxlevel . 1)
                             ("~/.personal/agenda/home.org" :maxlevel . 2)
                             ("~/.personal/agenda/work.org" :maxlevel . 2))
        org-refile-use-cache nil
        org-refile-use-outline-path nil
        org-startup-indented t
        org-startup-with-inline-images t
        org-tag-alist '((:startgroup . "Context")  ("@errands" . ?e)
                        ("@home" . ?h)
                        ("@work" . ?w)
                        (:endgroup)
                        (:startgroup . "Difficulty")
                        ("easy" . ?E)
                        ("medium" . ?M)
                        ("challenging" . ?C)
                        (:endgroup)
                        ("bug" . ?b)
                        ("car" . ?v)
                        ("future" . ?F)
                        ("goal" . ?g)
                        ("health" . ?H)
                        ("house" . ?O)
                        ("meeting" . ?m)
                        ("planning" . ?p)
                        ("phone" . ?0)
                        ("purchase" . ?P)
                        ("reading" . ?r)
                        ("review" . ?R)
                        ("study" . ?s)
                        ("sport" . ?S)
                        ("talk" . ?T)
                        ("tech" . ?t)
                        ("trip" . ?I)
                        ("thinking" . ?i)
                        ("update" . ?u)
                        ("watch" . ?l)
                        ("writing" . ?W))
        org-tags-exclude-from-inheritance '("crypt" "project")
        org-todo-keywords '((sequence "TODO(t)" "STARTED(s)"
                             "NEXT(n)"
                             "SOMEDAY(.)"
                             "WAITING(w)""|" "DONE(x!)" "CANCELLED(c@)") )
        org-use-effective-time t
        ;;  org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists
        org-yank-adjusted-subtrees t
        ;;  (map!
        ;;   :map org-mode-map
        ;;   "C-M-i" . completion-at-point)
        ;;  )
        ))

;;;; The built-in calendar mode mappings for org-journal
;;;; conflict with evil bindings
;;(map!
;; (:map calendar-mode-map
;;   :n "o" #'org-journal-display-entry
;;   :n "p" #'org-journal-previous-entry
;;   :n "n" #'org-journal-next-entry
;;   :n "O" #'org-journal-new-date-entry))

;; Local leader (<SPC m>) bindings for org-journal in calendar-mode
;; I was running out of bindings, and these are used less frequently
;; so it is convenient to have them under the local leader prefix
;;(map!
;; :map (calendar-mode-map)
;; :localleader
;; "w" #'org-journal-search-calendar-week
;; "m" #'org-journal-search-calendar-month
;; "y" #'org-journal-search-calendar-year)
;;
;;
(after! org
  (defvar my/org-active-task-template
    (concat "* NEXT %^{Task}\n"
            ":PROPERTIES:\n"
            ":Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END:") "Template for basic task.")
  (defvar my/org-appointment
    (concat "* TODO %^{Appointment}\n"
            "SCHEDULED: %t\n") "Template for appointment task.")
  (defvar my/org-basic-task-template
    (concat "* TODO %^{Task}\n"
            ":PROPERTIES:\n"
            ":Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END:") "Template for basic task.")
  (defvar my/org-contacts-template
    (concat "* %(org-contacts-template-name)\n"
            ":PROPERTIES:\n"
            ":BIRTHDAY: %^{YYYY-MM-DD}\n"
            ":END:") "Template for a contact.")
  (setq org-capture-templates
        `(
          ("c" "Contact" entry (file+headline "~/.personal/agenda/contacts.org" "Inbox"),
           my/org-contacts-template
           :empty-lines 1)
          ("p" "People" entry (file+headline "~/.personal/agenda/people.org" "Tasks"),
           my/org-basic-task-template
           :empty-lines 1)
          ("a" "Appointment" entry (file+headline "~/.personal/agenda/people.org" "Appointments"),
           my/org-appointment
           :empty-lines 1)
          ("m" "Meeting" entry (file+headline "~/.personal/agenda/people.org" "Meetings")
           "* Meeting with %? :meeting:\n%U" :clock-in t :clock-resume t :empty-lines 1)
          ("P" "Phone Call" entry (file+headline "~/.personal/agenda/people.org" "Phone Calls")
           "* Phone %? :phone:\n%U" :clock-in t :clock-resume t)

          ("i" "New Item")
          ("ib" "Book" checkitem (file+headline "~/.personal/items/books.org" "Books")
           "- [ ] %^{Title} - %^{Author}\n  %U"
           :immediate-finish t)
          ("il" "Learning" checkitem (file+headline "~/.personal/items/learning.org" "Things")
           "- [ ] %^{Thing}\n  %U"
           :immediate-finish t)
          ("im" "Movie" checkitem (file+headline "~/.personal/items/movies.org" "Movies")
           "- [ ] %^{Title}\n  %U"
           :immediate-finish t)
          ("ip" "Purchase" checkitem (file+headline "~/.personal/items/purchases.org" "Purchases")
           "- [ ] %^{Item}\n  %U"
           :immediate-finish t)

          ("t" "New Task")
          ("ta" "Active" entry (file+headline "~/.personal/agenda/inbox.org" "Active"),
           my/org-active-task-template
           :empty-lines 1
           :immediate-finish t)
          ("tb" "Backlog" entry (file+headline "~/.personal/agenda/inbox.org" "Backlog"),
           my/org-basic-task-template
           :empty-lines 1
           :immediate-finish t)))
  )

;;(setq org-capture-templates
;;      '(("w" "Work task" entry  (file+headline "~/org/tasks.org" "Work"))
;;        ("p" "Personal task" entry  (file+headline "~/org/tasks.org" "Personal"))
;;        ("d" "Dev task" entry  (file+headline "~/org/tasks.org" "Dev"))
;;        ("s" "Slipbox" entry  (file "~/org/inbox.org")
;;         "* %?\n")))

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("d" "dictionary" plain "%?"
         :if-new
         (file+head "dictionary/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))
(setq anajulia/default-bibliography `(,(expand-file-name "roam/biblio.bib" org-directory)))

(after! citar
  (map! :map org-mode-map
        :desc "Insert citation" "C-c b" #'citar-insert-citation)
  (setq citar-bibliography anajulia/default-bibliography
        citar-at-point-function 'embark-act
        citar-symbol-separator "  "
        citar-format-reference-function 'citar-citeproc-format-reference
        org-cite-csl-styles-dir "~/Zotero/styles"
        citar-citeproc-csl-styles-dir org-cite-csl-styles-dir
        citar-citeproc-csl-locales-dir "~/Zotero/locales"
        citar-citeproc-csl-style (file-name-concat org-cite-csl-styles-dir "apa.csl")))

;; (defun jethro/tag-new-node-as-draft ()
;;    (org-roam-tag-add '("draft")))
;;  (add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)
;;  (cl-defmethod org-roam-node-type ((node org-roam-node))
;;    "Return the TYPE of NODE."
;;    (condition-case nil
;;        (file-name-nondirectory
;;         (directory-file-name
;;          (file-name-directory
;;           (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;      (error "")))
;;  (setq org-roam-node-display-template
;;        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;  (require 'citar)
;;  (defun jethro/org-roam-node-from-cite (keys-entries)
;;    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
;;    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
;;                                                "${author editor} :: ${title}")))
;;      (org-roam-capture- :templates
;;                         '(("r" "reference" plain "%?" :if-new
;;                            (file+head "reference/${citekey}.org"
;;                                       ":PROPERTIES:
;;:ROAM_REFS: [cite:@${citekey}]
;;:END:
;;#+title: ${title}\n")
;;                            :immediate-finish t
;;                            :unnarrowed t))
;;                         :info (list :citekey (car keys-entries))
;;                         :node (org-roam-node-create :title props)
;;                         :finalize '(:title find-file))))


;; Portuguese dictionary
(with-eval-after-load "ispell"   (setq ispell-program-name "hunspell")   (setq ispell-dictionary "en_US,pt_BR")
                      ;; ispell-set-spellchecker-params has to be called
                      ;; before ispell-hunspell-add-multi-dic will work
                      (ispell-set-spellchecker-params)   (ispell-hunspell-add-multi-dic "en_US,pt_BR"))
