;; -*- lexical-binding: t -*-
;;; `setup-magit.el' --- Summary: Setup VCS packages
;;; Commentary:
;;; Code:

(use-package magit
  :commands magit-status
  :init (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-c h g" . hydra-magit/body)
   ("C-x m" . magit-status-fullscreen)
   ("C-x g" . magit-status))
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
  
  (set-default 'magit-diff-refine-hunk t)

  (defun magit-status-fullscreen (prefix)
    "Full screen magit-status.
  Unless PREFIX, delete other windows"
    (interactive "P")
    (magit-status-setup-buffer)
    (unless prefix
      (delete-other-windows)))

  (autoload 'magit-status-fullscreen "magit"))

;; update diff-hl (not installed)
;;(global-diff-hl-mode)
;;(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Navigate historic versions of the file
(use-package git-timemachine
  :straight (git-timemachine :host github :repo "emacsmirror/git-timemachine")
  :defer t)

;; Gitflow extension for magit
(use-package magit-gitflow
  :config
  :hook turn-on-magit-gitflow)

(use-package magithub
  :straight (magithub :type git :flavor melpa
                      :host github :repo "vermiculus/magithub")
  :commands magit-status)

(put 'magit-clean 'disabled nil)

(provide 'setup-magit)
;;; setup-magit.el ends here
