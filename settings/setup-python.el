;;; setup-python --- Setup Python editing -*- lexical-binding: t -*-

;; Author: Jonas Avrin
;; URL: https://www.github.com/jawabiscuit
;; Package-Requires: (`etom')

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

;;; Commentary:
;;
;; Requires `etom' for sending Python to Maya via socket connection

;;; Code:

;; Extensions to outline mode
(use-package outline-magic
  :bind (:map outline-minor-mode
	      ("<C-tab>" . outline-cycle)))

(use-package python-magic
  :after outline-magic
  :load-path "site-lisp"
  :straight (python-magic :type built-in))

;; Python major mode
(use-package python-mode
  :hook
  (((python-mode org-mode)
    . (lambda ()
        ;; Emacs to Maya
        (require 'etom)
        (setq etom-default-host "localhost")
        (setq etom-default-port 2222))))
  :bind (("C--" . outline-hide-body)
         ("C-=" . outline-show-all)
         ("C-+" . outline-cycle)
         ("C-c <C-return>" . etom-send-region)
         ("C-c C-S-c" . etom-send-buffer)
         ("C-c C-S-b" . etom-show-buffer)
         :map org-mode
         ("C-c <C-return>" . etom-send-region)
         ("C-c C-S-c" . etom-send-buffer)
         ("C-c C-S-b" . etom-show-buffer)
         ))


;; Automatically activates python virtualenv
(cond ((executable-find "virtualenvwrapper.sh")
       (use-package virtualenvwrapper
         :commands (venv-projectile-auto-workon
                    venv-workon
                    venv-mkvirtualenv-using
                    venv-mkvirtualenv
                    venv-rmvirtualenv
                    venv-lsvirtualenv
                    venv-cdvirtualenv
                    venv-cpvirtualenv))
       (use-package auto-virtualenvwrapper
         :hook
         ((python-mode . auto-virtualenvwrapper-activate))))
      (t (message "virtualenvwrapper was not found on the path.
 auto-virtualenvwrapper package was not loaded.")))

;; Python auto-formatter
(if (executable-find "autopep8")
    (use-package py-autopep8
      :ensure t
      :commands (py-autopep8-before-save py-autopep8-enable-on-save)
      ;; :hook
      ;; (python-mode . py-autopep8-enable-on-save)
      )
  (message "autopep8 was not found on the path.
 py-autopep8 package was not loaded."))

;; Minor mode for inserting docstring skeleton
(use-package sphinx-doc
  ;; C-c M-d
  :hook
  ((python-mode . sphinx-doc-mode)))

;; Minor mode that understands/highlights sphinx
(use-package python-docstring-mode
  ;; Seems buggy
  :disabled t
  :straight (python-docstring-mode
             :host github
             :repo "glyph/python-docstring-mode"))

;; Formatter for sorting imports
(use-package py-isort
  ;; Requires isort installation
  ;; :disabled t
  )

;; Emacs front-end for interacting with external debuggers like pdb
(use-package realgud)

;; Pip requirements with autocomplete
(use-package pip-requirements
  :hook
  ((pip-requirements-mode
    .
    pip-requirements-auto-complete-setup)))

(provide 'setup-python)
;;; setup-python.el ends here
