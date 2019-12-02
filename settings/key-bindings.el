;; Define hyper key on Windows
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

;; Lisp interaction
; `eval-last-sexp' is bound to `C-x' `C-e'.
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-defun)

;; C++
;; Use projectile command: C-c p a
;; (define-key c++-mode-map [f12] 'casey-find-corresponding-file)
;; M-S-f12
;; (define-key c++-mode-map [M-f12] 'casey-find-corresponding-file-other-window)

;; All custom global override key bindings
(defvar ja-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
      ;; Log and Todo Files
      (define-key map (kbd "M-t") 'load-todo)
      (define-key map (kbd "M-T") 'load-log)

      ;; Editing
      (define-key map (kbd "S-<tab>") 'indent-for-tab-command)
      (define-key map (kbd "C-<tab>") 'indent-region)
      (define-key map (kbd "C-d") 'duplicate-line)
      (define-key map (kbd "M-S-<up>") 'move-text-up)
      (define-key map (kbd "M-S-<down>") 'move-text-down)
      (define-key map (kbd "M-.") 'fill-paragraph)
      (define-key map (kbd "C-c <up>") 'rotate-yank-pointer)
      (define-key map (kbd "C-q") 'append-as-kill)
      (define-key map (kbd "C-Q") 'copy-region-as-kill)
      (define-key map (kbd "C-c h") 'casey-replace-string)
      (define-key map (kbd "C-c H") 'casey-replace-in-region)

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
      (define-key map (kbd "<f9>") 'first-error)
      (define-key map (kbd "<f10>") 'previous-error)
      (define-key map (kbd "<f11>") 'next-error)
      (define-key map (kbd "M-n") 'next-error)
      (define-key map (kbd "M-N") 'previous-error)
      (define-key map (kbd "M-g") 'goto-line)
      (define-key map (kbd "M-j") 'imenu)
      (define-key map (kbd "S-<down>") 'scroll-up-in-place)
      (define-key map (kbd "S-<up>") 'scroll-down-in-place)

      ;; Tools
      (define-key map (kbd "C-c C-q") 'quick-calc)
      ;; Window
      (define-key map (kbd "C-c M") 'maximize-frame)
      (define-key map (kbd "C-c m") 'minimize-window)
      ;; Selection
      (define-key map (kbd "C-=") 'er/expand-region)
      ;; Buffer
      (define-key map (kbd "M-r") 'revert-buffer)
      ;; Macro editing
      (define-key map (kbd "M-[") 'start-kbd-macro)
      (define-key map (kbd "M-]") 'end-kbd-macro)
      (define-key map (kbd "M-'") 'call-last-kbd-macro)

      ;; Counsel, Ivy, Swiper
      (define-key map (kbd "C-s") 'swiper)
      (define-key map (kbd "C-c C-r") 'ivy-resume)
      (define-key map (kbd "<f6>") 'ivy-resume)
      ;; Provided by `counsel-mode'
      ;; (define-key map (kbd "M-x") 'counsel-M-x)
      ;; (define-key map (kbd "C-x C-f") 'counsel-find-file)
      ;; (define-key map (kbd "<f1> f") 'counsel-describe-function)
      ;; (define-key map (kbd "<f1> v") 'counsel-describe-variable)
      ;; (define-key map (kbd "<f1> l") 'counsel-find-library)
      ;; (define-key map (kbd "<f2> i") 'counsel-info-lookup-symbol)
      (define-key map (kbd "H-u") 'counsel-unicode-char)
      ;; Find file in the current Git repository.
      (define-key map (kbd "C-c g") 'counsel-git)
      (define-key map (kbd "C-c j") 'counsel-git-grep)
      (define-key map (kbd "C-c k") 'counsel-ag)
      (define-key map (kbd "C-x l") 'counsel-locate)
      (define-key map (kbd "C-S-o") 'counsel-rhythmbox)

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
      (define-key map (kbd "C-c n") 'mc/mark-next-word-like-this)
      (define-key map (kbd "C-c N") 'mc/mark-previous-word-like-this)
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
      (define-key map (kbd "M-x") 'smex)
      (define-key map (kbd "M-X") 'smex-major-mode-commands)
    map)
  "ja-keys-minor-mode keymap")

(define-minor-mode ja-keys-minor-mode
  "A minor mode so that my key settings override all major modes
   with the added benefit of being able to turn off all at once"
  :init-value t
  :lighter " ja-keys")

(ja-keys-minor-mode 1)

;; Disable custom keys in the minibuffer
(defun ja-minibuffer-setup-hook ()
  (ja-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'ja-minibuffer-setup-hook)
;; End `ja-keys' minor mode setup

;; Undo tree
(eval-after-load 'undo-tree '(define-key undo-tree-map (kbd "C-?") nil))

;; Projectile
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Python magic
(define-key python-mode-map (kbd "C--") 'outline-hide-body)
(define-key python-mode-map (kbd "C-=") 'outline-show-all)
(define-key python-mode-map (kbd "C-+") 'outline-cycle)

;; Emacs to Maya
(define-key python-mode-map (kbd "<C-return>") 'etom-send-region)
(define-key python-mode-map (kbd "C-c C-c") 'etom-send-buffer)
(define-key python-mode-map (kbd "C-c C-l") 'etom-send-buffer)
(define-key python-mode-map (kbd "C-c C-z") 'etom-show-buffer)

;; Org Mode
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c c") 'org-capture)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
;TODO: make mnemonic
(define-key org-mode-map (kbd "C-c M-k") 'org-cut-subtree)
(define-key org-mode-map (kbd "C-c >") 'org-time-stamp-inactive)

;; Ask for a key then insert its html description
(define-key org-mode-map (kbd "C-c i") 'endless/insert-key)

;; Counsel, Ivy, Swiper
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Yasnippet
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

;; Bind `SPC' to `yas-expand' when snippet expansion available (it
;; will still call `self-insert-command' otherwise).
;; (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
;; Bind `C-c y' to `yas-expand' ONLY.
;; (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
;; End Yasnippet

(provide 'key-bindings)
