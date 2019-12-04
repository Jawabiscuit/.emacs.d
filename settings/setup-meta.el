(defvar jawa/feature-list
  '(setup-yasnippet           ; Snippets and templates
    setup-yankpad             ;
    setup-avy                 ;
))

(mapc #'jawa/require jawa/feature-list)

(provide 'setup-meta)
