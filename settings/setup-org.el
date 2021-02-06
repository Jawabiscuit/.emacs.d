;; -*- lexical-binding: t -*-
;;; `setup-org.el' --- Summary: Setup packages for using org-mode
;;; Commentary: Uses `general' and some packages in `setup-packages.el'
;;; Code:

;; `org-starter-path': "Load path" for Org files
(unless (bound-and-true-p org-starter-path)
  (setq org-starter-path `(,(abbreviate-file-name
                             (expand-file-name
                              "org-starter"
                              no-littering-etc-directory)))))

(use-package org-starter
  :defer t
  :diminish org-starter-mode
  :config
  ;; (org-starter-mode 1)
  (jawa/bind-user "t" 'org-starter-find-file-by-key)
  (general-add-hook 'org-starter-extra-find-file-map
                    '((";" org-starter-find-config-file "config")
                      ;; ("w" org-plain-wiki "wiki")
                      )
                    t)
  (general-add-hook 'org-starter-extra-alternative-find-file-map
                    '((";" org-starter-swiper-config-files "config")
                      ;; ("w" helm-org-rifle-wiki "wiki/writing")
                      )
                    t)
  (general-add-hook 'org-starter-extra-refile-map
                    '(("'" avy-org-refile-as-child "avy")
                      ("?" akirak/org-refile-same-buffer "same buffer"))
                    t)
  ;; Force load configuration files loaded from `org-starter-path'
  ;; Just setting `org-starter-load-config-files' (the var) isn't
  ;; doing it on it's own anymore
  (org-starter-load-config-files)

  ;; Use `org-starter' with swiper
  (use-package org-starter-swiper)

  (use-package org-starter-extras
    :straight (org-starter-extras :host github :repo "akirak/org-starter"
                                  :files ("org-starter-extras.el")))
  :custom
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))

  ;; (org-starter-alternative-find-file-command #'helm-org-rifle-files)
  (org-starter-find-file-visit-window t)
  (org-starter-override-agenda-window-setup 'other-window)
  (org-starter-enable-local-variables :all))

(use-package org-reverse-datetree
  :defer t)

(use-package org-ql
  :straight (org-ql :type git :flavor melpa :host github :repo "alphapapa/org-ql")
  :commands (org-ql-search org-ql-view org-ql-view-sidebar org-ql-view-recent-items))

;; Library for working with xml via esxml and sxml
(use-package esxml
  :mode "\\.xml\\'")

;; Compatible layer for URL request in Emacs
(use-package request
  :defer t)

;; Functions and commands useful for retrieving web page content and processing it into
;; Org-mode content
(use-package org-web-tools
  :defer t)

(use-package org-link-edit
  :straight (org-link-edit :host github :repo "kyleam/org-link-edit")
  :defer t)

;; Pretty bullets :) instead of ugly asterisks :(
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
)

;; Bootstrap compatible HTML Back-End for Org
(use-package ox-twbs
  :defer t)

;; Group items in Org agenda views
(use-package org-super-agenda
  :config
  (setq org-agenda-custom-commands '())
  (add-to-list 'org-agenda-custom-commands
   '("H" "Someday/Maybe"
     ((tags-todo "SCHEDULED>=\"<tomorrow>\"")
      (todo "INCUBATE"))
     (;;(org-agenda-overriding-header "Someday/Maybe tasks")
      (org-agenda-sorting-strategy '(priority-up effort-down))
      ;; (org-super-agenda-groups
      ;;  '((:auto-map my-org-super-agenda-group-by-project-or-task-group)))
     )
     ("someday-maybe.html")))

  (defun my-org-super-agenda ()
    (interactive)
    (let ((org-super-agenda-groups
            '((:log t)
              (:name "Schedule"
                     :time-grid t)
              ;; Group habit items (items which have a STYLE: habit Org property).
              (:habit t)
              (:name "Tag"
                     :auto-tags)
              (:name "Due Today"
                     :deadline today)
              (:name "Overdue"
                     :deadline past)
              (:name "Due Soon"
                     :deadline future)
              (:name "Waiting"
                     :todo "WAITING"
                     ;; A number setting the order sections will be displayed in the agenda, lowest number first. Defaults to 0.
                     :order 98)
              (:name "Someday/Maybe"
                     :tag ("someday")
                     :order 100)
              (:name "Scheduled Earlier"
                     :scheduled past))))
      (org-agenda-list)))

  ;; (org-super-agenda-mode 1)

  :bind (("C-c h o" . hydra-org/body)
         ("C-c O" . my-org-super-agenda)))

;; Capture notes from pdfs and sync with org
(use-package org-noter
  :defer t)

;; Facilitate moving images from point A to point B
(use-package org-download
  :straight (:type git
             :flavor melpa
             :host github
             :repo "abo-abo/org-download")
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable)))

;; Asynchronous src_block evaluation for org-babel
(use-package ob-async
  :straight (ob-async
             :type git
             :flavor melpa
             :host github
             :repo "astahlman/ob-async")
  :defer t)

(use-package org-edna
  :straight (org-edna :host github :repo "akirak/org-edna" :branch "edit")
  :defer t
  :config
  (org-edna-load))

;; Crushes table editing problems
;; Archived, may need to disable
(use-package tabcrush
  :straight (tabcrush :host github :repo "raxod502/tabcrush")
  :defer t)

;; Active Babel languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (C . t)
     (python . t)
     (ditaa . t)
     (shell . t)
     )))

;; Hide leading asterisks and indent correctly
(setq org-hide-leading-stars t)

;; Use syntax highlighting in source blocks while editing
(setq org-src-fontify-natively t)

;; Make TAB act as if it were issued in a buffer of the language's
;; major mode. 
(setq org-src-tab-acts-natively nil)

;; When editing a code snippet, use the current window rather than
;; popping open a new one (which shows the same information). 
(setq org-src-window-setup 'current-window)

;; Indentation for the content of a source code block.
;; It has no effect if `org-src-preserve-indentation' is non-nil.
(setq org-edit-src-content-indentation 0
      org-src-preserve-indentation t)

;; Post org v9.2 Requirement for structure templates
(with-eval-after-load 'org
  (require 'org-tempo))

;; GTD TODO keywords and hide logs
(setq org-todo-keywords
      '((sequence
         "TODO"
         "ACTION"
         "IN-PROGRESS"
         "INCUBATE"
         "DEFERRED(@d)"
         "WAITING(w@)"
         "|"
         "DONE(@o)"
         "DELEGATED(l@)"
         "ARCHIVE"
         "CANCELLED"
         )))

(setq org-log-into-drawer nil)

;; tasks.el
(add-hook 'org-checkbox-statistics-hook 'jawa/org-checkbox-todo)

;; GTD fast tag selection
(setq org-tag-persistent-alist '(
   ("gtd" . ?G)
   (:startgroup)
   ("engage" . ?N)  ;; Day to day engagement
   ("review" . ?R)  ;; Periodic review
   ("someday" . ?S) ;; Someday maybe project list
   (:endgroup)
   
   ;; Three Models for making action choices
   
   ;; 1 - The Four-Criteria Model for choosing actions in the moment
   
     ("context" . ?C)
     ;; 1 - Context : are you in the right space to do this action?
     (:startgroup)
     ("@home" . ?h) ("@work" . ?w) ("@anywhere" . ?a) ("@mobile")
     (:endgroup)
     
     ("sub_context" . ?X)
     (:startgroup)
     ("office" . ?1) ("outside" . ?2) ("garage" . ?3)
     ("kitchen" . ?4) ("bathroom" . ?5) ("storage" . ?6)
     (:endgroup)
     
     ("status" . ?B)
     (:startgroup)
     ;; ("status" . ?X))
     ("online". ?o) ("offline" . ?O)
     (:endgroup)
     
     ("type" . ?E)
     (:startgroup)
     ("meeting" . ?m) ("discussion" . ?t) ("call" . ?c)
     (:endgroup)
     
     ;; 2 - Time Available : do you have enough time to complete it?
     ("time" . ?T)
     (:startgroup)
     ("5m_or_less" . ?q)  ; quick
     ("30m_or_less" . ?l)  ; less quick
     ("30m_or_more" . ?s)  ; slow
     (:endgroup)
     
     ;; 3 - Energy available : are you alert enough to do this?
     ("intensity" . ?I)
     (:startgroup)
     ("high" . ?9)
     ("low" . ?0)
     (:endgroup)
     
     ;; 4 - Priority : what's going to give you the highest payoff
     ;; Track this using TODO priority
     
   ;; 2 - The Threefold Model for Identifying Daily Work
   ;; Doing predefined work - working from NAs and calendar
   ;; Do work as it shows up
   ;; Defining your work - clearing inboxes, processing meeting notes, breaking down new projects
   ;; Do during periodic review meeting
   ;; Track this using :review: tag
     
   ;; 3 - The Six-Level Model for Reviewing Your Own Work
     
     ;; There are 6 perspectives to define priorities
     
     ;; 1 - Ground : current next actions list
     
     ;; 2 - Projects : Current projects, they are generating the most NAs
     ("project" . ?p)("area" . ?A)
     (:startgroup)
     ("clarify" . ?y)
     ("brainstorm" .?b)
     ("reference" . ?r)
     ("research" . ?j)
     (:endgroup)
     
     ;; 3 - Areas of Focus and Accountability : key areas of life and work.
     ;; TODO Identify areas of focus
     ("aof" . ?k)
     (:startgroup)
     (:endgroup)
     
     ;; 4 - Goals : one to two years from now
     ;; TODO Identify goals
     ("goals" . ?g)
     (:startgroup)
     (:endgroup)
     
     ;; 5 - Vision : projecting three to five years out into bigger categories
     ("vision" . ?v)
     (:startgroup)
     ("strategies" . ?z)
     ("trends" . ?d)
     ("career" . ?e)
     (:endgroup)
     
     ;; 6 - Purpose and principles : Big picture view
     ;; TODO Identify principles
     ("principles" . ?i)
     (:startgroup)
     (:endgroup)
))

;; Format timestamps
;; First element formats date
;; Second element formats date + time
;; (custom-set-variables
;;  '(org-display-custom-times t)
;;  '(org-time-stamp-custom-formats (
;;     ;; EDT (summer months)
;;     ;; quote (" %Y-%m-%d " . " %Y-%m-%d %H:%M:%S -0400 "))))
;;     ;; EST
;;     quote (" %Y-%m-%d " . " %Y-%m-%d %H:%M:%S %z "))))

;; Ellipsis
(setq org-ellipsis "»")
(custom-set-faces '(org-ellipsis ((t (:underline nil)))))

;; Todo Tasks and Agenda
(setq org-columns-default-format "%60ITEM(Task) %10Effort(Effort){:} %PRIORITY %10CLOCKSUM(T Spent) %10CLOCKSUM_T(T Spent Today) %TAGS")
(setq org-global-properties
      (quote (("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
              ("SYTLE_ALL" . "habit"))))

;; Keep track of when a certain Todo item was finished
(setq org-log-done 'time
      ;; record a note along with the timestamp
      ;; org-log-done 'note
      ;; hide markup elements
      org-hide-emphasis-markers t)

(pretty-hydra-define+ hydra-org ()
  ("Web"
  (("fj" org-link-edit-forward-slurp "forward slurp")
   ("fk" org-link-edit-forward-barf "forward barf")
   ("fu" org-link-edit-backward-slurp "backward slurp")
   ("fi" org-link-edit-backward-barf "backward barf")
   ("fr" jk/unlinkify "remove link"))))

;; Make clock history persist across sessions
;; (docs) ...the incomplete clock will be found (see Resolving idle time) and
;; you will be prompted about what to do with it
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(provide 'setup-org)
;;; setup-org.el ends here
