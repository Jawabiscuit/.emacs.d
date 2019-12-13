;; A utility package to collect various Icon Fonts and propertize them within Emacs.
(use-package all-the-icons
  :general
  ;; ("C-x 8 i" #'all-the-icons-ivy)
)

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
        counsel-projectile-find-dir))
)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'setup-all-the-icons)
