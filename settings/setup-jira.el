;;; setup-jira --- Jira integration settings -*- lexical-binding: t -*-

;; Author: Jonas Avrin
;; URL: https://www.github.com/jawabiscuit
;; Package-Requires: (`')

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

;;; Commentary:
;;
;;

;;; Code:

(use-package org-jira
  :straight (org-jira
             :type git
             :host github
             :repo "ahungry/org-jira")
  :config (setq jiralib-url "https://jira.blueskystudios.com"))


(use-package ejira
  :disabled t
  :straight (ejira :type git :host github :repo "nyyManni/ejira")
  :init
  (setq jiralib2-url              "https://jira.blueskystudios.com"
        jiralib2-auth             'basic
        jiralib2-user-login-name  "jonasavrin"
        jiralib2-token

        ejira-org-directory       "~/org/jira"
        ejira-projects            '("CHARDEV")

        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?B)
                                    ("Medium"  . ?C)
                                    ("Low"     . ?D)
                                    ("Lowest"  . ?E))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Done"        . 3)))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda)

  ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
  ;; into your `org-agenda-files'.
  (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view to browse the issues that
  (org-add-agenda-custom-command
   '("j" "My JIRA issues"
     ((ejira-jql "resolution = unresolved and assignee = currentUser()"
                 ((org-agenda-overriding-header "Assigned to me")))))))

(provide 'setup-jira)
;;; setup-jira.el ends here
