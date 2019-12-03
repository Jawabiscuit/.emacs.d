(defvar jawa/feature-list
  '(setup-yasnippet           ; Snippets and templates
    setup-yankpad             ;
))

(mapc #'jawa/require jawa/feature-list)

(provide 'setup-meta)
