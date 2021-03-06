;; -*- lexical-binding: t -*-
;;; `setup-delimiters.el' --- Summary: Setup for parens and delimiters
;;; Commentary:
;;; Code:

(use-package smartparens
  ;; Unclutter the modeline
  :diminish smartparens-mode
  :init
  (setq sp-autoescape-string-quote nil)
  :hook ((css-mode . smartparens-mode)
         (restclient-mode . smartparens-mode)
         (js-mode . smartparens-mode)
         (java-mode . smartparens-mode)
         (ruby-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)
         (emacs-lisp-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (python-mode . smartparens-mode)
         (c++-mode . smartparens-mode)
         (css-mode . show-smartparens-mode)
         (restclient-mode . show-smartparens-mode)
         (js-mode . show-smartparens-mode)
         (java-mode . show-smartparens-mode)
         (ruby-mode . show-smartparens-mode)
         (markdown-mode . show-smartparens-mode)
         (emacs-lisp-mode . show-smartparens-mode)
         (org-mode . show-smartparens-mode)
         (python-mode . show-smartparens-mode)
         (c++-mode . show-smartparens-mode))
  :config
  (require 'smartparens-config)

  ;; elisp comment highlighting
  ;; Redefine this global pair (`LaTex') to a new value locally
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  
  ;; New Org pairs
  ;; (sp-local-pair 'org-mode "=" "=" :wrap "C-+")
  ;; (sp-local-pair 'org-mode "<" ">" :wrap "C-<")

  ;; Yas pairs
  ;; (sp-pair "${" ":}" :wrap "C-$")
  
  ;; Local pairs can be removed by calling `sp-local-pair' with optional keyword argument `:actions' with value `:rem'
  ;; (sp-local-pair 'LaTeX-mode "`" nil :actions :rem)
  (sp-local-pair 'org-mode "<" ">" :actions :rem)

  ;; Create a wrapping
  ;; Usage
  ;;
  ;; |foobar
  ;; hit C-(
  ;; becomes (|foobar)
  (sp-pair "(" ")" :wrap "C-(") ; `sp-wrap-round' does this without having to define
  :bind ("C-c h d" . hydra-parens/body))

;; Display nested sets of delimiters in different colors
(use-package rainbow-delimiters
  :straight (rainbow-delimiters :host github :repo "Fanael/rainbow-delimiters")
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (python-mode . rainbow-delimiters-mode)))

;; Display ugly form feed characters as tidy horizontal rules
(use-package page-break-lines
  ;; :straight (page-break-lines
  ;;            :host github
  ;;            :repo "purcell/page-break-lines"
  ;;            )
  :config (page-break-lines-mode))

(provide 'setup-delimiters)
;;; setup-delimiters.el ends here
