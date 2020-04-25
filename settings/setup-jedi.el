;;; setup-jedi -- Python auto-completion  -*- lexical-binding: t -*-

;; Author: Jonas Avrin
;; URL: https://www.github.com/jawabiscuit
;; Package-Requires: (`python-mode')

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
;;

;;; Code:

;; System constants
(require 'system-const)

(use-package python-environment)

;; The brains of Python autocomplete
(use-package emacs-jedi
  :straight (emacs-jedi :host github :repo "tkf/emacs-jedi")
  :init
  ;; Uncomment next line if you like the menu right away
  (setq ac-show-menu-immediately-on-auto-complete t)
  ;; Can also express in terms of ac-delay var, e.g.:
  ;;   (setq ac-auto-show-menu (* ac-delay 2))
  ;; Enable Jedi setup on mode start
  (general-add-hook 'python-mode-hook 'jedi:setup)
  :config
  ;; Don't let tooltip show up automatically
  (setq jedi:get-in-function-call-delay 10000000)
  ;; Start completion at method dot
  (setq jedi:complete-on-dot t)
  ;; Custom location where jedi is installed
  (if sys/bss-hostname-p
      (setq jedi:environment-root "jedi"
            python-environment-directory "~/.virtualenvs"))
  :mode "\\.py\\'"
  :mode-hydra
  (python-mode
   (:tite "Jedi Commands")
   ("Goto"
    (("." jedi:goto-definition "Symbol definition")
     ("," jedi:goto-definition-pop-marker "Symbol definition, pops marker"))
    "Docs"
    (("?" jedi:show-doc "Show docstring")
     ("/" jedi:get-in-function-call "Manually show call signature")))))

(provide 'setup-jedi)
;;; setup-jedi.el ends here
