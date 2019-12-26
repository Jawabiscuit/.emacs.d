;; A utility package to collect various Icon Fonts and propertize them within Emacs.
;; NOTE: It is necessary to install fonts manually on Windows after they are downloaded.
(use-package all-the-icons)

(use-package all-the-icons-ivy
  ;; Integrate it with ivy-filthy-rich
  ;; :disabled t
  :after ivy
  ;; :straight (all-the-icons-ivy :host github :repo "asok/all-the-icons-ivy")
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
      '(counsel-find-file
        counsel-file-jump
        counsel-recentf
        counsel-projectile-find-file
        counsel-projectile-find-dir)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'setup-all-the-icons)
