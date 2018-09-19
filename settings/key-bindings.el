;; Magit
(global-set-key (kbd "C-x m") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")

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

(provide 'key-bindings)
