(setq project-dir default-directory)
(if (file-exists-p (concat project-dir ".emacs.desktop"))
  (desktop-change-dir default-directory))

