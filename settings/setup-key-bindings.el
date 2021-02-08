;; Define hyper key on Windows
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

;; Lisp interaction
; `eval-last-sexp' is bound to `C-x' `C-e'.
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-defun)

;; All custom global override key bindings
(defvar ja-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Editing
    ;; (define-key map (kbd "S-<tab>") 'indent-for-tab-command)
    (define-key map (kbd "C-<tab>") 'indent-region)
    (define-key map (kbd "C-d") 'duplicate-line)
    (define-key map (kbd "M-S-<up>") 'move-text-up)
    (define-key map (kbd "M-S-<down>") 'move-text-down)
    (define-key map (kbd "C-c <up>") 'rotate-yank-pointer)
    ;; (define-key map (kbd "C-Q") 'copy-region-as-kill)
    (define-key map (kbd "M-s r") 'replace-string)

    ;; Navigation
    ;; (define-key map (kbd "M-w") 'other-window)
    ;; (define-key map (kbd "C-<right>") 'forward-word)
    ;; (define-key map (kbd "C-<left>") 'backward-word)
    (define-key map (kbd "C-<up>") 'previous-blank-line)
    (define-key map (kbd "C-<down>") 'next-blank-line)
    (define-key map (kbd "C-<next>") 'scroll-other-window) ; next = page down
    (define-key map (kbd "C-<prior>") 'scroll-other-window-down) ; prior = page up
    (define-key map (kbd "M-j") 'imenu)
    (define-key map (kbd "S-<down>") 'scroll-up-in-place)
    (define-key map (kbd "S-<up>") 'scroll-down-in-place)

    ;; Macro editing
    (define-key map (kbd "M-[") 'start-kbd-macro)
    (define-key map (kbd "M-]") 'end-kbd-macro)
    (define-key map (kbd "M-'") 'call-last-kbd-macro)

    map)
  "ja-keys-minor-mode keymap")

(define-minor-mode ja-keys-minor-mode
  "A minor mode so that my key settings override all major modes
   with the added benefit of being able to turn off all at once"
  :init-value nil
  :lighter " jaK")

(diminish 'ja-keys-minor-mode)
(ja-keys-minor-mode nil)
(jawa/bind-user "j" 'ja-keys-minor-mode)

;; Disable custom keys in the minibuffer
(defun ja-minibuffer-setup-hook ()
  (ja-keys-minor-mode 0))

(general-add-hook 'minibuffer-setup-hook 'ja-minibuffer-setup-hook)
;; End `ja-keys' minor mode setup

;; Navigating
(jawa/bind-jump "d" 'dired-jump)
(jawa/bind-user ":" 'View-back-to-mark)
(jawa/bind-user ";" 'exchange-point-and-mark)

;; Org-mode
(jawa/bind-jump "k" 'org-clock-goto)
(jawa/bind-user "C-x 4" 'jawa/org-archive-subtree-as-completed)

;; Tools
(jawa/bind-user "q" 'quick-calc)

;; Window
(jawa/bind-user "M" 'maximize-frame)
(jawa/bind-user "m" 'minimize-frame)
;; Buffer
(jawa/bind-user "C-x r" 'revert-buffer)
(jawa/bind-user "C-x k" 'kill-this-buffer)

;;Search
(jawa/bind-search "r" 'rgrep)

;; Selection
(jawa/bind-user "=" 'er/expand-region)
;; Especially helpful when using Emacs terminal UI
(general-define-key
 :keymaps 'org-mode-map
 "C-^" 'er/expand-region)

;; Editing
(jawa/bind-user "c" 'copy-region-as-kill)
(jawa/bind-user "v" 'kill-region)
(jawa/bind-user "'" 'quoted-insert)
(jawa/bind-user "M-." 'fill-paragraph)

;; Split all kinds of org blocks
;; https://scripter.co/splitting-an-org-block-into-two/
(jawa/bind-user "W" 'modi/org-split-block)

;; Undo tree
(eval-after-load 'undo-tree '(define-key undo-tree-map (kbd "C-?") nil))

;; Ask for a key then insert its html description
(define-key org-mode-map (kbd "C-c i") 'endless/insert-key)

;; Org src edit
(eval-after-load 'org-src '(define-key org-src-mode-map (kbd "C-x C-s") #'org-edit-src-exit))

(provide 'setup-key-bindings)
