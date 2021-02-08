;; -*- lexical-binding: t -*-
;;; `setup-projects.el' --- Summary: Project management
;;; Commentary:
;;; Code:

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(use-package projectile
  :defer t
  :diminish
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  (add-to-list 'projectile-globally-ignored-directories ".pyc")

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))
    ;; Too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

  ;; Update mode-line at the first time
  (projectile-update-mode-line)

  (use-package counsel-projectile)
  :bind
  (("C-c h p" . hydra-projectile/body)
   :map projectile-mode-map
   ("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map)))

(provide 'setup-projects)
;;; setup-projects.el ends here
