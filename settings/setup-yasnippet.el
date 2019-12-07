(use-package yasnippet
  ;; :diminish 'yas-minor-mode
  :config
  (defun jawa/visit-yas-snippet-dir ()
    (interactive)
    (find-file (car yas-snippet-dirs)))
  ;; (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  ;; (add-hook 'org-mode-hook #'yas-minor-mode)
  ;; (add-hook 'python-mode-hook #'yas-minor-mode)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :load-path "site-lisp/yasnippet-snippets"
  :straight (yasnippet-snippets :type built-in)
  :init
  ;; By default, the snippets in this package are loaded at package
  ;; initialization.
  ;;
  ;; This is annoying since the snippets override my own snippets
  ;; when they conflict.
  ;;
  ;; To prevent this issue, I replace `yasnippet-snippets-initialize'
  ;; with an alternative version that doesn't load the snippets.
  ;;
  ;; Instead, I call `yas-reload-all' after this package is loaded.
  ;; This works as expected since this package is loaded after
  ;; `yasnippet' package.
  (defun jawa/yasnippet-snippets-initialize-no-reload ()
    "Add the directory but don't reload the snippets."
    (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t))
  (advice-add 'yasnippet-snippets-initialize
              :override
              'jawa/yasnippet-snippets-initialize-no-reload)
  :config
  ;; Load all snippets. You need to make sure that all snippet
  ;; directories are added beforehand.
  (yas-reload-all))

(use-package auto-yasnippet
  ;; :commands (aya-create aya-expand)
)

(use-package ivy-yasnippet
  ;; When you use the :commands keyword, it creates autoloads for
  ;; those commands and defers loading of the module until they are used
  :commands (ivy-yasnippet)
  :config
  (jawa/bind-user "y" 'ivy-yasnippet)
  (jawa/bind-register "n" 'yas-new-snippet))

(provide 'setup-yasnippet)
