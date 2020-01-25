;;; setup-org.el --- Setup packages for org-mode  -*- lexical-binding: t -*-
;;
;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`')
;; Homepage:
;; Keywords:
;;
;;
;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; (defun myorg-update-parent-cookie ()
;;   (when (equal major-mode 'org-mode)
;;     (save-excursion
;;       (ignore-errors
;;         (org-back-to-heading)
;;         (org-update-parent-todo-statistics)))))
;;
;; (defadvice org-kill-line (after fix-cookies activate)
;;   (myorg-update-parent-cookie))
;;
;; (defadvice kill-whole-line (after fix-cookies activate)
;;   (myorg-update-parent-cookie))

;; Library for working with xml via esxml and sxml
(use-package esxml)

;; Compatible layer for URL request in Emacs
(use-package request)

;; Functions and commands useful for retrieving web page content and processing it into
;; Org-mode content
(use-package org-web-tools)

(use-package org-link-edit
  :straight (org-link-edit :host github :repo "kyleam/org-link-edit"))

;; Pretty bullets :) instead of ugly asterisks :(
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
)

;; Bootstrap compatible HTML Back-End for Org
(use-package ox-twbs)

;; Group items in Org agenda views
(use-package org-super-agenda
  :after org-agenda
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

  (org-super-agenda-mode 1)

  :bind (("C-c h o" . hydra-org/body)
         ("C-c O" . my-org-super-agenda)))

;; Capture notes from pdfs and sync with org
(use-package org-noter)

;; Active Babel languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (C . t)
     (python . t)
     (ditaa . t)
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

;; BEGIN structure templates
(require 'org-tempo)

;; Elisp block
(add-to-list 'org-structure-template-alist
  '("sl" . "src emacs-lisp\n"))

;; Python block
(add-to-list 'org-structure-template-alist
  '("sp" . "src python\n"))

;; Ditaa export args
(add-to-list 'org-structure-template-alist
  '("d" . "src ditaa :file ?.png :exports results\n"))

;; More tag
(add-to-list 'org-structure-template-alist
  '("hm" . "export html\n<!--more-->"))

;; Edit on Github div
(add-to-list 'org-structure-template-alist
  '("hg" . "export html\n<div class=\"right\">\n  <a href=\"https://github.com/fniessen/org-html-themes/blob/master/demo/example.org\" class=\"fa fa-github\"> Edit on GitHub</a>\n</div>"))

;; Jekyll post front matter
(add-to-list 'org-structure-template-alist
  '("hj" . "export html\n@@html:---\nlayout: post\ntitle: \ndate: \ncategory: \nauthor: Jonas Avrin\n---\n@@"))

;; END structure templates

;; GTD TODO keywords and hide logs
(setq org-todo-keywords
      '((sequence "TODO" "ACTION" "IN-PROGRESS" "INCUBATE" "DEFERRED(@d)" "WAITING(w@)" "|" "DONE(@o)" "DELEGATED(l@)" "ARCHIVE")))
(setq org-log-into-drawer 1)

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
      org-log-done 'note
      ;; hide markup elements
      org-hide-emphasis-markers t)

;; Org mode links
;; `https://github.com/abo-abo/hydra/wiki/Org-mode-links'
(defun jk/unlinkify ()
  "Replace an org-link with the description, or if this is absent, the path."
  (interactive)
  (let ((eop (org-element-context)))
    (when (eq 'link (car eop))
      (message "%s" eop)
      (let* ((start (org-element-property :begin eop))
             (end (org-element-property :end eop))
             (contents-begin (org-element-property :contents-begin eop))
             (contents-end (org-element-property :contents-end eop))
             (path (org-element-property :path eop))
             (desc (and contents-begin
                        contents-end
                        (buffer-substring contents-begin contents-end))))
        (setf (buffer-substring start end)
              (concat (or desc path)
                      (make-string (org-element-property :post-blank eop) ?\s)))))))

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
