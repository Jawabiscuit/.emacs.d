(setq project-dir default-directory)
(if (file-exists-p (concat project-dir ".emacs.desktop"))
    (desktop-change-dir default-directory))

;; (if (file-exists-p (concat project-dir "org/todo.org"))
;;     (add-to-list 'org-agenda-files (concat project-dir "org/todo.org")))

