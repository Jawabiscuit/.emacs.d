;; Define hyper key on Windows
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

;; Lisp interaction
; `eval-last-sexp' is bound to `C-x' `C-e'.
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-defun)

;; All custom global override key bindings
(defvar ja-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
      ;; Log and Todo Files
      (define-key map (kbd "M-t") 'load-todo)
      (define-key map (kbd "M-T") 'load-log)

      ;; Editing
      ;; (define-key map (kbd "S-<tab>") 'indent-for-tab-command)
      (define-key map (kbd "C-<tab>") 'indent-region)
      (define-key map (kbd "C-d") 'duplicate-line)
      (define-key map (kbd "M-S-<up>") 'move-text-up)
      (define-key map (kbd "M-S-<down>") 'move-text-down)
      (define-key map (kbd "M-.") 'fill-paragraph)
      (define-key map (kbd "C-c <up>") 'rotate-yank-pointer)
      (define-key map (kbd "C-Q") 'copy-region-as-kill)
      (define-key map (kbd "M-s r") 'replace-string)

      ;; Navigation
      (define-key map (kbd "M-w") 'other-window)
      (define-key map (kbd "C-<right>") 'forward-word)
      (define-key map (kbd "C-<left>") 'backward-word)
      (define-key map (kbd "C-<up>") 'previous-blank-line)
      (define-key map (kbd "C-<down>") 'next-blank-line)
      (define-key map (kbd "C-<next>") 'scroll-other-window)          ; next = page down
      (define-key map (kbd "C-<prior>") 'scroll-other-window-down)    ; prior = page up    
      (define-key map (kbd "M-:") 'View-back-to-mark)
      (define-key map (kbd "M-;") 'exchange-point-and-mark)
      (define-key map (kbd "M-j") 'imenu)
      (define-key map (kbd "S-<down>") 'scroll-up-in-place)
      (define-key map (kbd "S-<up>") 'scroll-down-in-place)

      ;; Buffer
      (define-key map (kbd "M-r") 'revert-buffer)
      (define-key map (kbd "C-x k") 'kill-this-buffer)
      
      ;; Macro editing
      (define-key map (kbd "M-[") 'start-kbd-macro)
      (define-key map (kbd "M-]") 'end-kbd-macro)
      (define-key map (kbd "M-'") 'call-last-kbd-macro)

      ;; Experimental multiple-cursors
      (define-key map (kbd "C-S-c C-S-c") 'mc/edit-lines)
      (define-key map (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
      (define-key map (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
      
      ;; Mark additional regions matching current region
      (define-key map (kbd "C-x W") 'mc/mark-all-dwim)
      (define-key map (kbd "C-x w") 'mc/mark-all-in-region)
      (define-key map (kbd "C-c R") 'mc/mark-previous-like-this)
      (define-key map (kbd "C-c r") 'mc/mark-next-like-this)
      (define-key map (kbd "C-c x") 'mc/mark-more-like-this-extended)
      
      ;; Symbol and word specific mark-more
      ;; (define-key map (kbd "C-c n") 'mc/mark-next-word-like-this)
      ;; (define-key map (kbd "C-c N") 'mc/mark-previous-word-like-this)
      (define-key map (kbd "C-<f3>") 'mc/mark-all-words-like-this)
      (define-key map (kbd "C-c s") 'mc/mark-next-symbol-like-this)
      (define-key map (kbd "C-c S") 'mc/mark-previous-symbol-like-this)
      (define-key map (kbd "C-<f4>") 'mc/mark-all-symbols-like-this)
      
      ;; Extra multiple cursors stuff
      (define-key map (kbd "C-~") 'mc/reverse-regions)
      (define-key map (kbd "M-~") 'mc/sort-regions)
      (define-key map (kbd "H-~") 'mc/insert-numbers)
      (define-key map (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
      (define-key map (kbd "C-c C-a") 'mc/mark-all-like-this)

      ;; Magit
      (define-key map (kbd "C-x m") 'magit-status-fullscreen)
      (define-key map (kbd "C-x g") 'magit-status)

      ;; Smex
      ;; (define-key map (kbd "M-x") 'smex)
      ;; (define-key map (kbd "M-X") 'smex-major-mode-commands)
    map)
  "ja-keys-minor-mode keymap")

(define-minor-mode ja-keys-minor-mode
  "A minor mode so that my key settings override all major modes
   with the added benefit of being able to turn off all at once"
  :init-value t
  :lighter " jaK")

(diminish 'ja-keys-minor-mode)

(ja-keys-minor-mode 1)
;; Disable custom keys in the minibuffer
(defun ja-minibuffer-setup-hook ()
  (ja-keys-minor-mode 0))

(general-add-hook 'minibuffer-setup-hook 'ja-minibuffer-setup-hook)
;; End `ja-keys' minor mode setup

;; Tools
(jawa/bind-user "q" 'quick-calc)

;; Window
(jawa/bind-user "M" 'maximize-frame)
(jawa/bind-user "m" 'minimize-frame)

;; Selection
(jawa/bind-user "=" 'er/expand-region)

;; Editing
(jawa/bind-user "c" 'copy-region-as-kill)
(jawa/bind-user "v" 'kill-region)
(jawa/bind-user "'" 'quoted-insert)

;; Undo tree
(eval-after-load 'undo-tree '(define-key undo-tree-map (kbd "C-?") nil))

;; Ask for a key then insert its html description
(define-key org-mode-map (kbd "C-c i") 'endless/insert-key)

(provide 'setup-key-bindings)
