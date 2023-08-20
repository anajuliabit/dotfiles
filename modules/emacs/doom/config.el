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
              ("C-<tab>" . 'copilot-accept-completion-by-word))
        :config
        ;; turn on by default
        (copilot-mode 1)
        ;; use company as a fallback
        (setq copilot-use-company-fallback t)
  )


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
        org-agenda-span 6
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
        org-hide-emphasis-markers t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-track-ordered-property-with-tag t
        org-log-into-drawer t
        org-log-state-notes-into-drawer t
        org-log-repeat 'time
        org-todo-repeat-to-state "TODO"
        +org-capture-todo-file "inbox.org"
        +org-capture-notes-file "inbox.org"
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
        org-refile-targets '((org-agenda-files :maxlevel . 3)
                             ("~/.personal/agenda/home.org" :maxlevel . 5)
                             ("~/.personal/agenda/work.org" :maxlevel . 5))
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
                             "WAITING(w)""|" "DONE(d!)" "CANCELLED(c@)") )
        org-use-effective-time t
        ;;  org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists
        org-yank-adjusted-subtrees t
        ;;  (map!
        ;;   :map org-mode-map
        ;;   "C-M-i" . completion-at-point)
        ;;  )
        )
                                        ; Make calendars in agenda start on Monday
  (setq calendar-week-start-day 1)
  (setq         ;;org-habit-completed-glyph ?✓
   org-habit-graph-column 80
   org-habit-preceding-days 35
   ;;org-habit-today-glyph ?‖
   org-habit-show-habits t
   org-habit-show-all-today t
   )
  (defun +org-habit-resize-graph-h nil)
  )


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
    (concat "* %^{Name}\n"
            ":PROPERTIES:\n"
            ":EMAIL: %^{Email}\n"
            ":PHONE: %^{Phone}\n"
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
          ("tw" "Work" entry (file+headline "~/.personal/agenda/work.org" "Work"),
           my/org-basic-task-template
           :empty-lines 1
           :immediate-finish t)
          ("th" "Home" entry (file+headline "~/.personal/agenda/home.org" "Home"),
           my/org-basic-task-template
           :empty-lines 1
           :immediate-finish t)))
  )

(after! org-clock
  (defun my/org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read
              "Effort: "
              (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))
  (add-hook 'org-clock-in-prepare-hook 'my/org-mode-ask-effort)
  (setq org-clock-clocktable-default-properties
        '(:block thisweek :maxlevel 2 :scope agenda :link t :compact t :formula %
                 :step week :fileskip0 t :stepskip0 t :narrow 50
                 :properties ("Effort" "CLOCKSUM" "TODO"))
        org-clock-continuously nil
        org-clock-in-switch-to-state "STARTED"
        org-clock-out-remove-zero-time-clocks t
        org-clock-persist t
        org-clock-persist-file (expand-file-name "~/.cache/emacs/org-clock-save.el")
        org-clock-persist-query-resume nil
        org-clock-report-include-clocking-task t
        org-show-notification-handler (lambda (msg) (alert msg))))

(after! org-pomodoro
  (setq alert-user-configuration '((((:category . "org-pomodoro")) libnotify nil))
        org-pomodoro-start-sound-p t
        org-pomodoro-length 25
        org-pomodoro-short-break-length 5
        ))

;;(after! org-contacts
 ;; (setq org-contacts-files '("~/.personal/agenda/contacts.org")))

(after! org-roam
  (setq my/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n"
        my/daily-note-filename "%<%Y-%m-%d>.org"
        )
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
           :unnarrowed t))
        org-roam-completion-everywhere t
        org-roam-dailies-directory "journal/"
        org-roam-dailies-capture-templates
        `(("d" "default" plain
           "* %?"
           :if-new (file+head ,my/daily-note-filename
                              ,my/daily-note-header)
           :empty-lines 1)

          ("j" "journal" plain
           "** %<%I:%M %p>  :journal:\n\n%?\n\n"
           :if-new (file+head+olp ,my/daily-note-filename
                                  ,my/daily-note-header
                                  ("Journal"))
           :empty-lines 1)
          ("m" "meeting" entry
           "** %<%I:%M %p> - %^{Meeting Title}  :meeting:\n\n%?\n\n"
           :if-new (file+head+olp ,my/daily-note-filename
                                  ,my/daily-note-header
                                  ("Meetings"))
           :empty-lines 1))
        org-roam-directory "~/.personal/notes"
        org-roam-graph-viewer "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
        )
  )

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

(setq auth-sources '("~/.authinfo.gpg"))


(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))

(use-package! lsp-mode
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "classes")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\minio\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\terraform\\'")
  ;; fix "bug"
  (advice-remove #'lsp #'+lsp-dont-prompt-to-install-servers-maybe-a)

  (advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers))))


 (use-package! lsp-grammarly
   :defer t ; Even though the hook implies defer still add it for clarity
   :commands lsp-grammarly-resume
    :hook ((text-mode . lsp)
         (markdown-mode . lsp))
   :init
   (setq lsp-grammarly-domain "technical"
         lsp-grammarly-audience "expert"))

(use-package! keytar
  :defer t
  :config
  (setq keytar-install-dir (concat doom-data-dir "keytar")))

;;(use-package! eglot-grammarly
;;  :after eglot
;;  :init
;;  (setq eglot-grammarly-active-modes +grammarly-enabled-modes))

(use-package! define-it
  :defer t
  :commands define-it-at-point
  :config
  (setq define-it-show-google-translate t
        define-it-show-header nil
        google-translate-default-source-language "auto"
        google-translate-default-target-language "pt_BR"))

;; Jump to buffer when results are fetched
(defun define-it--find-buffer (x)
  (let ((buf (format define-it--buffer-name-format define-it--current-word)))
    (pop-to-buffer buf)))

(advice-add 'define-it--in-buffer :after #'define-it--find-buffer)

;; Set popup rule to define-it
(after! define-it
  (set-popup-rule! "\\*define-it:" :side 'right))
