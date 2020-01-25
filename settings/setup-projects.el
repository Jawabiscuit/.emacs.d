;;; setup-projects.el --- Configure project management tools -*- lexical-binding: t -*-

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

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(use-package projectile
  :diminish
  :bind ("C-c h p" . hydra-projectile/body)
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  ;; Update mode-line at the first time
  ;; (projectile-update-mode-line)

  ;; TODO: Use the faster searcher to handle project files: ripgrep `rg'.
  ;; (when (and (not (executable-find "fd"))
  ;;            (executable-find "rg"))
  ;;   (setq projectile-generic-command
  ;;         (let ((rg-cmd ""))
  ;;           (dolist (dir projectile-globally-ignored-directories)
  ;;             (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
  ;;           (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  ;; (when sys/win32p
  ;;   (when (or (executable-find "fd") (executable-find "rg"))
  ;;     (setq projectile-indexing-method 'alien
  ;;           projectile-enable-caching nil))

  ;; TODO: too slow while getting submodule files on Windows
  ;; (setq projectile-git-submodule-command nil))
  :bind
  (:map projectile-mode
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))

(provide 'setup-projects)
;;; setup-projects.el ends here
