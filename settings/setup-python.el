;;; `setup-python.el' --- Summary: Setup python IDE functionality
;;; Commentary:
;;; Code:

;; Python + elpy configuration
(use-package python
  :straight (python
             :type git
             :host github
             :repo "emacs-straight/python")
  :mode
  (("\\.py$" . python-mode)
   ("\\.wsgi$" . python-mode))
  :config
  ;; Elpy
  (use-package elpy
    :straight (elpy
               :type git
               :flavor melpa
               :files ("*.el" "NEWS.rst" "snippets" "elpy" "elpy-pkg.el")
               :host github
               :repo "jorgenschaefer/elpy")
    :commands elpy-enable
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    (add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))
    (with-eval-after-load 'python (elpy-enable))
    :config
    (setq elpy-rpc-backend "jedi")
    :bind
    (:map elpy-mode-map
	  ("M-." . elpy-goto-definition)
	  ("M-," . pop-tag-mark)))

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
         )
    :config
    (message "python-mode configured!"))

  (use-package jedi-direx
    ;; elpy-occur-definitions
    :disabled t
    :after python-mode
    :straight (jedi-direx :type git :flavor melpa
                          :host github :repo "tkf/emacs-jedi-direx")
    :bind (:map python-mode
                ("C-c x" . jedi-direx:pop-to-buffer))
    :hook (python-mode . jedi-direx:setup)
    :config
    (message "jedi-direx configured!"))
  
  (use-package python-magic
    :load-path "site-lisp"
    :straight (python-magic :type built-in))
)


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
  :disabled t)

;; Emacs front-end for interacting with external debuggers like pdb
(use-package realgud
  :commands realgud:pdb)

;; Pip requirements with autocomplete
(use-package pip-requirements
  :hook
  ((pip-requirements-mode .
    pip-requirements-auto-complete-setup)))

(provide 'setup-python)
;;; setup-python.el ends here
