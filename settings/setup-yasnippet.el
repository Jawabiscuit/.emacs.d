;;; `setup-yasnippet.el' --- Setup snippet packages -*- lexical-binding: t -*-
;;
;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`')
;; Homepage:
;; Keywords:
;;
;;
;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package yasnippet
  ;; :diminish 'yas-minor-mode
  ;; :bind (("<tab>" . nil) ;; Unbind Tab completion
  ;;        ("TAB" . nil)
  ;;        ;; Bind `SPC' to `yas-expand' when snippet expansion available (it
  ;;        ;; will still call `self-insert-command' otherwise).
  ;;        ("SPC" . yas-maybe-expand)
  ;;        ;; Bind `C-c y' to `yas-expand' ONLY.
  ;;        ("C-c y" . #'yas-expand))
  :config
  (defun jawa/visit-yas-snippet-dir ()
    (interactive)
    (find-file (car yas-snippet-dirs)))
  (yas-global-mode 1)
  ;; :hook (emacs-lisp-mode org-mode python-mode)
)

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
  :bind (("C-c y" . ivy-yasnippet)
         ("C-c n" . yas-new-snippet)))

(provide 'setup-yasnippet)
;;; setup-yasnippet.el ends here
