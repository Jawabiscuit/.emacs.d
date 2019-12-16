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

(use-package org-link-edit
  :straight (org-link-edit :host github :repo "kyleam/org-link-edit"))

;; Pretty bullets :) instead of ugly asterisks :(
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)
)

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

;; BEGIN structure templates

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

;; Ellipsis
(setq org-ellipsis "»")
(custom-set-faces '(org-ellipsis ((t (:underline nil)))))

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

(provide 'setup-org)
