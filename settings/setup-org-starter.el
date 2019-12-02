;; `org-starter-path': "Load path" for Org files
(unless (bound-and-true-p org-starter-path)
  (setq org-starter-path `(,(abbreviate-file-name
                             (expand-file-name
                              "org-starter"
                              no-littering-etc-directory)))))

(use-package org-starter
  :config
  (org-starter-mode 1)
  (org-starter-def "~/.emacs.d/configuration.org"
    :key "m"
    :refile (:maxlevel . 5))
  ;; (org-starter-def "~/home.nix/README.org"
  ;;   :key "n"
  ;;   :refile (:maxlevel . 3))
  (general-add-hook 'org-starter-extra-find-file-map
                    '((";" org-starter-find-config-file "config")
                      ("w" org-plain-wiki "wiki"))
                    t)
  ;; (general-add-hook 'org-starter-extra-alternative-find-file-map
  ;;                   '((";" org-starter-swiper-config-files "config")
  ;;                     ("w" helm-org-rifle-wiki "wiki/writing"))
  ;;                   t)
  ;; (general-add-hook 'org-starter-extra-refile-map
  ;;                   '(("'" avy-org-refile-as-child "avy")
  ;;                     ("?" akirak/org-refile-same-buffer "same buffer"))
  ;;                   t)
  :custom
  ;; Enable external configuration files loaded from `org-starter-path'
  (org-starter-load-config-files t)
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  ;; (org-starter-alternative-find-file-command #'helm-org-rifle-files)
  (org-starter-find-file-visit-window t)
  (org-starter-override-agenda-window-setup 'other-window)
  (org-starter-enable-local-variables :all))

;; Use `org-starter' with swiper
(use-package org-starter-swiper)

(provide 'setup-org-starter)
