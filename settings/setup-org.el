;; (defun myorg-update-parent-cookie ()
;;   (when (equal major-mode 'org-mode)
;;     (save-excursion
;;       (ignore-errors
;;         (org-back-to-heading)
;;         (org-update-parent-todo-statistics)))))

;; (defadvice org-kill-line (after fix-cookies activate)
;;   (myorg-update-parent-cookie))

;; (defadvice kill-whole-line (after fix-cookies activate)
;;   (myorg-update-parent-cookie))

(setq org-directory "~/Documents/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; keep track of when a certain TODO item was finished
(setq org-log-done 'time)

;; record a note along with the timestamp
;; (setq org-log-done 'note)

;; Active Babel languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (C . t)
     (python . t)
     (ditaa . t)
)))

;; Pretty bullets :) instead of ugly asterisks :(
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)
)

;; Hide leading asterisks and indent correctly
(setq org-hide-leading-stars t)

;; Use syntax highlighting in source blocks while editing
(setq org-src-fontify-natively t)

;; Make TAB act as if it were issued in a buffer of the language's
;; major mode. 
(setq org-src-tab-acts-natively t)

;; When editing a code snippet, use the current window rather than
;; popping open a new one (which shows the same information). 
(setq org-src-window-setup 'current-window)

;; This blog was really helpful figuring structure templates out
;; https://blog.aaronbieber.com/2016/11/23/creating-org-mode-structure-templates.html

(add-to-list 'org-structure-template-alist
             (list "d" (concat "#+BEGIN_SRC ditaa :file ?.png :exports results\n"
                               "#+end_src")))

(add-to-list 'org-structure-template-alist
             (list "m" (concat "#+BEGIN_EXPORT html\n"
                               "<!--more-->\n"
                               "#+END_EXPORT\n\n")))

;; Edit on Github
(add-to-list 'org-structure-template-alist
             (list "g" (concat "#+BEGIN_EXPORT html\n"
                               "<div class=\"right\">\n"
                               "  <a href=\"https://github.com/fniessen/org-html-themes/blob/master/demo/example.org\" class=\"fa fa-github\"> Edit on GitHub</a>\n"
                               "</div>\n"
                               "#+END_EXPORT\n")))

;; Quickly insert a block of elisp
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; https://orgmode.org/manual/Easy-templates.html
;; Create a new structure template
(add-to-list 'org-structure-template-alist
             (list "p" (concat ":PROPERTIES:\n"
                               ":CATEGORY: ?\n"
                               ":END:")))

(add-to-list 'org-structure-template-alist
             (list "py" (concat "#+BEGIN_SRC python\n"
                                "?\n"
                                "#+END_SRC")))

;; Html export options template
(add-to-list 'org-structure-template-alist
             (list "eh" (concat ":EXPORT_FILE_NAME: ?\n"
                                ":EXPORT_TITLE:\n"
                                ":EXPORT_OPTIONS: toc:nil html-postamble:nil num:nil")))

;; Org mode header settings template
(add-to-list 'org-structure-template-alist
             (list "o" (concat "#+OPTIONS: title:nil toc:nil ^:nil num:nil html-postamble:nil tasks:nil\n"
                               "#+STARTUP: content indent\n"
                               "#+STARTUP: hidestars\n"
                               "#+AUTHOR: Jonas Avrin\n"
                               "#+TITLE: ?\n"
                               "#+SUBTITLE: \n"
                               "#+DESCRIPTION: \n"
                               "#+TAGS: \n"
                               "#+EXPORT_FILE_NAME: ")))

;; Org mode in-buffer Org header settings template
(add-to-list 'org-structure-template-alist
             (list "os" (concat ":SETTINGS:\n"
                                "#+OPTIONS: title:nil toc:nil ^:nil num:nil html-postamble:nil tasks:nil\n"
                                "#+STARTUP: content indent\n"
                                "#+STARTUP: hidestars\n"
                                "#+AUTHOR: Jonas Avrin\n"
                                "#+TITLE: ?\n"
                                "#+SUBTITLE: \n"
                                "#+DESCRIPTION: \n"
                                "#+TAGS: \n"
                                "#+EXPORT_FILE_NAME: \n"
                                ":END:")))

;; Org mode in-buffer Jekyll header settings template
(add-to-list 'org-structure-template-alist
             (list "js" (concat ":SETTINGS:\n"
                                "#+OPTIONS: title:nil toc:nil ^:nil num:nil html-postamble:nil tasks:nil\n"
                                "#+STARTUP: content indent\n"
                                "#+STARTUP: hidestars\n"
                                "#+AUTHOR: Jonas Avrin\n"
                                "#+TITLE: ?\n"
                                "#+SUBTITLE: \n"
                                "#+DESCRIPTION: \n"
                                "#+TAGS: \n"
                                "#+EXPORT_FILE_NAME: \n"
                                "@@html:---\n"
                                "layout: post\n"
                                "title: \n"
                                "date: \n"
                                "category: \n"
                                "tags: \n"
                                "author: Jonas Avrin\n"
                                "---\n"
                                "@@\n"
                                ":END:")))

;; Org mode in-buffer RTD header settings template
;; OPTIONS key/values:
;; H:   -> the number of headline levels for export
;; ^:{} -> 'a^{b}' will be interpreted as superscript
;; _:{} -> 'a_{b}' will be interpreted as subscript
;; \n:  -> preserve line breaks
;; ::   -> fixed width sections
(add-to-list 'org-structure-template-alist
             (list "rs" (concat ":SETTINGS:\n"
                                "#+OPTIONS: H:2 toc:2 ^:{} _:{} \\n:nil ::t\n"
                                "#+OPTIONS: title:t num:nil html-postamble:t tasks:t\n"
                                "#+STARTUP: content indent\n"
                                "#+STARTUP: hidestars\n"
                                "#+STARTUP: showall\n"
                                "#+TITLE: ?\n"
                                "#+AUTHOR: Jonas Avrin\n"
                                "#+DATE: \n"
                                "#+EMAIL: jonas@cghijinks.com\n"
                                "#+SUBTITLE: \n"
                                "#+DESCRIPTION: \n"
                                "#+TAGS: \n"
                                "#+EXPORT_FILE_NAME: ../html\n"
                                "#+SETUPFILE: ../themes/setup/theme-readtheorg.setup\n"
                                ":END:")))

;; Jekyll post front matter
(add-to-list 'org-structure-template-alist
             (list "j" (concat "#+BEGIN_HTML\n"
                               "@@html:---\n"
                               "layout: post\n"
                               "title: ?\n"
                               "date: \n"
                               "category: \n"
                               "author: Jonas Avrin\n"
                               "---\n"
                               "@@\n"
                               "#+END_HTML")))

;; GTD TODO keywords and hide logs
(setq org-todo-keywords
      '((sequence "TODO" "ACTION" "INCUBATE" "DEFERRED" "WAITING(w@)" "|" "DONE" "DELEGATED" "ARCHIVE")))
(setq org-log-into-drawer 1)

;; GTD fast tag selection
(setq org-tag-persistent-alist '(("gtd" . ?G)
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
                        ("project" . ?p)
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
(custom-set-variables
 '(org-display-custom-times t)
 '(org-time-stamp-custom-formats (
    ;; EDT (summer months)
    ;; quote (" %Y-%m-%d " . " %Y-%m-%d %H:%M:%S -0400 "))))
    ;; EST
    quote (" %Y-%m-%d " . " %Y-%m-%d %H:%M:%S -0500 "))))

;; Capture Templates
(setq org-capture-templates
  '(("t" "Task" entry (file (concat org-directory "/inbox.org"))
     "** TODO %?\n")
    ("p" "Project" entry (file+headline (concat org-directory "/projects.org" "Projects"))
     (file (concat org-directory "/templates/project-template.org")))
    ("l" "Log" entry (file+headline (concat org-directory "/log.org" "Log"))
     (file (concat org-directory "/templates/log-template.org"))))
)

;; Ellipsis
(setq org-ellipsis "»")
(custom-set-faces '(org-ellipsis ((t (:underline nil)))))

(provide 'setup-org)
