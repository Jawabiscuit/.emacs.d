;; -*- lexical-binding: t -*-
;;; `setup-jira.el' --- Summary: Jira settings
;;; Commentary:
;;; Code:
(jawa/require 'system-const)

(if sys/bss-hostname-p (setq jira-url "https://jira.blueskystudios.com"
                             jira-login-name "jonasavrin"
                             jira-projects-list '("CHARDEV", "MEDIA"))
  (setq jira-url ""
        jira-login-name ""
        jira-projects-list '()))

(use-package org-jira
  :straight (org-jira
             :type git
             :host github
             :repo "ahungry/org-jira")
  :config (setq jiralib-url jira-url))

(if sys/bss-hostname-p
    (use-package ejira
      :straight (ejira :type git :host github :repo "nyyManni/ejira")
      :init
      (setq jiralib2-url              jira-url
            jiralib2-auth             'basic
            jiralib2-user-login-name  jira-login-name
            jiralib2-token

            ejira-org-directory       "~/org/jira"
            ejira-projects            jira-projects-list

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
                     ((org-agenda-overriding-header "Assigned to me"))))))))

(provide 'setup-jira)
;;; setup-jira.el ends here
