;; -*- lexical-binding: t -*-
;;; `setup-gitgutter.el' --- Summary: Git-gutter configuration
;;; Commentary:
;;; Code:



;; Git-gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :general
  ("C-x v s" #'git-gutter:stage-hunk)
  ("C-x v r" #'git-gutter:revert-hunk)
  ("C-x v =" #'git-gutter:popup-hunk)
  ("C-x v SPC" #'git-gutter:mark-hunk)
  :custom
  ;; If you set `git-gutter:update-interval' seconds larger than 0,
  ;; git-gutter updates diff information in real-time by idle timer. 
  (custom-set-variables
   '(git-gutter:update-interval 2))
  
  ;; You can change the signs and those faces
  (custom-set-variables
   '(git-gutter:modified-sign "  ") ;; two space
   '(git-gutter:added-sign "++")    ;; multiple character is OK
   '(git-gutter:deleted-sign "--"))
  ;; (set-face-background 'git-gutter:modified "purple") ;; background color
  ;; (set-face-foreground 'git-gutter:added "green")
  ;; (set-face-foreground 'git-gutter:deleted "red")

  ;; first character should be a space
  (custom-set-variables
   '(git-gutter:lighter " GG"))

  ;; Emacs has char-width function which returns character width.
  ;; `git-gutter.el' uses it for calculating character length of the signs.
  ;; But char-width does not work for some full-width characters. So you
  ;; should explicitly specify window width, if you use full-width
  ;; character.
  ;; (custom-set-variables
  ;;  '(git-gutter:window-width 2)
  ;;  '(git-gutter:modified-sign "?")   ; cloud
  ;;  '(git-gutter:added-sign "?")      ; sun
  ;;  '(git-gutter:deleted-sign "?"))   ; umbrella
  
  ;; Use for 'Git'(`git'), 'Mercurial'(`hg'), 'Bazaar'(`bzr'), and
  ;; 'Subversion'(`svn') projects
  (custom-set-variables
   '(git-gutter:handled-backends '(git hg bzr svn)))

  ;; deactivate `git-gutter-mode' in `asm-mode' and `image-mode'
  ;; (custom-set-variables
  ;;  '(git-gutter:disabled-modes '(asm-mode image-mode)))

  ;; Emacs folds long line if `truncate-lines' is nil. If
  ;; git-gutter:visual-line is non-nil, git-gutter puts sign by visual
  ;; lines.
  ;; (custom-set-variables
  ;;  '(git-gutter:visual-line t))
  
  ;; `git-gutter.el' can view unchanged information by setting
  ;; git-gutter:unchanged-sign. Like following. 
  ;; (custom-set-variables
  ;;  '(git-gutter:unchanged-sign " "))
  ;; (set-face-background 'git-gutter:unchanged "yellow")
  
  ;; `git-gutter.el' can display an additional separator character at the
  ;; right of the changed signs. This is mostly useful when running
  ;; emacs in a console. 
  ;; (custom-set-variables
  ;;  '(git-gutter:separator-sign "|"))
  ;; (set-face-foreground 'git-gutter:separator "yellow")
  
  ;; Hide gutter when there are no changes if `git-gutter:hide-gutter' is
  ;; non-nil. (Default is nil)
  (custom-set-variables
   '(git-gutter:hide-gutter t))
  
  ;; You can pass git diff option to set `git-gutter:diff-option'
  ;; ignore all spaces
  (custom-set-variables
   '(git-gutter:diff-option "-w"))
  
  ;; `git-gutter.el' always asks you whether `commit/revert' or not. If you
  ;; don't want, please set `git-gutter:ask-p' to `nil'.
  (custom-set-variables
   '(git-gutter:ask-p nil))
  
  ;; Don't need log/message.
  (custom-set-variables
   '(git-gutter:verbosity 4))  ; 0-4, 4 being more verbose and the default

  :config
  ;; Enable global minor mode
  (global-git-gutter-mode t)

  ;; Jump to next/previous hunk
  (jawa/bind-jump
    "j" 'git-gutter:next-hunk
    "k" 'git-gutter:previous-hunk)
  
  ;; diff information is updated at hooks in git-gutter:update-hooks.
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  
  ;; diff information is updated after command in
  ;; git-gutter:update-commands executed.
  (add-to-list 'git-gutter:update-commands 'other-window)
  
  ;; If you want to display line numbers globally, always couple this
  ;; with (global-git-gutter-mode t)
  ;; (global-display-line-numbers-mode t)
  
  ;; Enable `git-gutter-mode' for some modes
  ;; (add-hook 'ruby-mode-hook 'git-gutter-mode)

)

;; Don't do this!
;; run (global-git-gutter-mode t) AFTER
;; (global-display-line-numbers-mode t)
;; https://github.com/syohex/emacs-git-gutter/issues/156
;; If you would like to use git-gutter.el and linum-mode
;;(git-gutter:linum-setup)


(provide 'setup-gitgutter)
