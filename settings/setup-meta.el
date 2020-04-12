(defvar jawa/feature-list
  '(setup-yasnippet           ; Snippets and templates
    setup-avy                 ; Jump to things in emacs tree-style
    setup-yankpad             ; Paste snippets from an org-mode file
))

(mapc #'jawa/require jawa/feature-list)

(provide 'setup-meta)
