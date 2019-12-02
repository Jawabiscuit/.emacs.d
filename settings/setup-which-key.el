(use-package which-key
  ;; :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  ;; Display a popup window at bottom
  (which-key-setup-side-window-bottom)
)

(defmacro jawa/which-key-add-stripped-prefix (prefix)
  "Add PREFIX as a stripped prefix to `which-key-replacement-alist'."
  `(add-to-list 'which-key-replacement-alist
                (quote ((nil . ,prefix) .
                        (lambda (kb)
                          (cons (car kb)
                                (string-remove-prefix ,prefix (cdr kb))))))))

(jawa/which-key-add-stripped-prefix "jawa/")

(provide 'setup-which-key)
