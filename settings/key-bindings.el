;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")
(global-set-key (kbd "C-x g") 'magit-status)

;; Experimental multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; Mark additional regions matching current region
(global-set-key (kbd "C-x W") 'mc/mark-all-dwim)
(global-set-key (kbd "C-x w") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c R") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c r") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c x") 'mc/mark-more-like-this-extended)

;; Symbol and word specific mark-more
(global-set-key (kbd "C-c n") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-c N") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-<f3>") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-c s") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "C-c S") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "C-<f4>") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-~") 'mc/reverse-regions)
(global-set-key (kbd "M-~") 'mc/sort-regions)
(global-set-key (kbd "H-~") 'mc/insert-numbers)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Log and Todo Files
(define-key global-map "\et" 'load-todo)
(define-key global-map "\eT" 'load-log)

;; Editing
(define-key global-map [f8] 'casey-replace-string)
(define-key global-map "\eq" 'append-as-kill)
(define-key global-map "" 'copy-region-as-kill)
(define-key global-map "" 'rotate-yankpointer)
(define-key global-map "\eu" 'undo)
(define-key global-map "\e6" 'upcase-word)
(define-key global-map "\e^" 'captilize-word)
(define-key global-map "\el" 'casey-replace-in-region)
(define-key global-map "\eo" 'query-replace)
(define-key global-map "\eO" 'casey-replace-string)
(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map [C-tab] 'indent-region)
(define-key global-map "	" 'indent-region)
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)
;(define-key global-map [M-delete] 'kill-word)
;(define-key global-map "\377" 'backward-kill-word)  ; \377 is alt-backspace

;; Tools
(define-key global-map "\ep" 'quick-calc)

;; Navigation
(define-key global-map "\ew" 'other-window)
(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [C-next] 'scroll-other-window)
(define-key global-map [C-prior] 'scroll-other-window-down)
(define-key global-map "\e:" 'View-back-to-mark)
(define-key global-map "\e;" 'exchange-point-and-mark)
(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)
(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)
(define-key global-map "\eg" 'goto-line)
(define-key global-map "\ej" 'imenu)

;; Macro editing
(define-key global-map "\e[" 'start-kbd-macro)
(define-key global-map "\e]" 'end-kbd-macro)
(define-key global-map "\e'" 'call-last-kbd-macro)

;; Buffer
(define-key global-map "\er" 'revert-buffer)

;; Fold-this
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Org Mode
(global-set-key (kbd "C-c l") 'org-store-link)

;; Undo tree
(eval-after-load 'undo-tree '(define-key undo-tree-map (kbd "C-?") nil))

;; Projectile
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'key-bindings)
