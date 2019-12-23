;;; `setup-delimiters.el' --- Setup for parens and delimiters -*- lexical-binding: t -*-
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

(use-package smartparens
  ;; Unclutter the modeline
  :diminish smartparens
  :init
  (setq sp-autoescape-string-quote nil)
  :config
  (require 'smartparens-config)
  (--each '(css-mode-hook
            restclient-mode-hook
            js-mode-hook
            java-mode-hook
            ruby-mode-hook
            markdown-mode-hook
            emacs-lisp-mode-hook
            org-mode-hook
            python-mode-hook
            c++-mode-hook)
    (add-hook it 'turn-on-smartparens-mode)
    (add-hook it 'turn-on-show-smartparens-mode))

  ;; elisp comment highlighting
  ;; Redefine this global pair (`LaTex') to a new value locally
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (sp-local-pair 'org-mode "=" "=" :wrap "C-+")
  
  ;; New pair
  (sp-pair "<" ">" :wrap "C-<")
  
  ;; Local pairs can be removed by calling `sp-local-pair' with optional keyword argument `:actions' with value `:rem'
  ;; (sp-local-pair 'LaTeX-mode "`" nil :actions :rem)
  
  ;; Create a wrapping
  ;; Usage
  ;;
  ;; |foobar
  ;; hit C-(
  ;; becomes (|foobar)
  (sp-pair "(" ")" :wrap "C-(") ; `sp-wrap-round' does this without having to define
  :bind ("C-c h d" . hydra-delims/body))

;; Display nested sets of delimiters in different colors
(use-package rainbow-delimiters
  :straight (rainbow-delimiters :host github :repo "Fanael/rainbow-delimiters")
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (python-mode . rainbow-delimiters-mode)))

;; Display ugly form feed characters as tidy horizontal rules
(use-package page-break-lines
  :straight (page-break-lines :host github :repo "purcell/page-break-lines"))

(provide 'setup-delimiters)
;;; setup-delimiters.el ends here
