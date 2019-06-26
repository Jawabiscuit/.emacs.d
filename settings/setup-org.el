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

;; (setq org-directory "~/Dropbox/org")
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map (kbd "M-<f6>") 'org-capture)

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

;; Quickly insert a block of elisp
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; TODO: Enable spell-check in org-mode
; (add-hook 'org-mode-hook 'flyspell-mode)

;; This blog was really helpful figuring structure templates out
;; https://blog.aaronbieber.com/2016/11/23/creating-org-mode-structure-templates.html

;; Create a new structure template
(add-to-list 'org-structure-template-alist
             (list "p" (concat ":PROPERTIES:\n"
                               "?\n"
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

;; Org mode header options template
(add-to-list 'org-structure-template-alist
             (list "o" (concat "#+OPTIONS: title:nil toc:nil ^:nil num:nil\n"
                               "#+STARTUP: content indent\n"
                               "#+STARTUP: hidestars\n"
                               "#+AUTHOR: Jonas Avrin\n"
                               "#+TITLE: \n"
                               "#+SUBTITLE: \n"
                               "#+DESCRIPTION: \n")))

;; Jekyll post front matter
(add-to-list 'org-structure-template-alist
             (list "j" (concat "@@html:---\n"
                               "layout: post\n"
                               "title: ?\n"
                               "date: \n"
                               "category: \n"
                               "---\n"
                               "@@\n")))

(provide 'setup-org)
