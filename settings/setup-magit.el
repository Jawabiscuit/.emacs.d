;;; setup-magit.el --- Setup version control packages -*- lexical-binding: t -*-
;;
;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`')
;; Homepage:
;; Keywords:
;;
;;
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
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun magit-status-fullscreen (prefix)
  "Full screen magit-status.
  Unless PREFIX, delete other windows"
  (interactive "P")
  (magit-status-setup-buffer)
  (unless prefix
    (delete-other-windows)))

(autoload 'magit-status-fullscreen "magit")

(use-package magit
  :init (setq magit-completing-read-function 'ivy-completing-read)
  :bind (("C-c h g" . hydra-magit/body))
  :hook ((git-commit-mode . jawa/magit-cursor-fix))
  :config
  ;; Don't prompt me
  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes))
  
  (defun jawa/magit-cursor-fix ()
    "Move cursor into position when entering a commit message."
    (goto-char (point-min))
    (when (looking-at "#")
      (forward-line 2)))
    
  (defun vc-annotate-quit ()
    "Restore the previous window configuration and kill the `vc-annotate' buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :vc-annotate-fullscreen))
  
  (defvar vc-annotate-mode-map)
  (eval-after-load "vc-annotate"
    '(progn
       (defadvice vc-annotate (around fullscreen activate)
         (window-configuration-to-register :vc-annotate-fullscreen)
         ad-do-it
         (delete-other-windows))
       (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))
  
  (set-default 'magit-diff-refine-hunk t))

;; update diff-hl (not installed)
;;(global-diff-hl-mode)
;;(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Navigate historic versions of the file
(use-package git-timemachine
  :straight (git-timemachine :host github :repo "emacsmirror/git-timemachine"))

;; Gitflow extension for magit
(use-package magit-gitflow
  :config
  :hook turn-on-magit-gitflow)

(use-package jedi-direx
  :straight (jedi-direx :type git :flavor melpa
                        :host github :repo "tkf/emacs-jedi-direx"))

(use-package magithub
  :straight (magithub :type git :flavor melpa
                      :host github :repo "vermiculus/magithub"))

(put 'magit-clean 'disabled nil)

(provide 'setup-magit)
;;; setup-magit.el ends here
