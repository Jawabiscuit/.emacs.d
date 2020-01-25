;;; `setup-auto-complete.el' --- setup auto complete package -*- lexical-binding: t -*-
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

(use-package fuzzy
  :straight (fuzzy :host github :repo "auto-complete/fuzzy-el"))

(use-package auto-complete
  :diminish ac-mode
  :commands (ac-config-default)
  :bind
  ("C-c /" . ac-fuzzy-complete)
  :config
  ;; (dolist (ac-mode '(text-mode org-mode))
  ;;   (add-to-list 'ac-modes ac-mode))

  (dolist (ac-mode-hook '(text-mode-hook org-mode-hook prog-mode-hook))
    (add-hook ac-mode-hook
              (lambda ()
                (setq ac-fuzzy-enable t)
                (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
                (add-to-list 'ac-sources 'ac-source-filename))))

  ;; Known bug
  (ac-flyspell-workaround)
  (global-auto-complete-mode))

;; Middleware so python-land can communicate with emacs-land
(use-package epc)

;; Brains of Python auto-complete
;; IMPORTANT: Jedi not fully compatible with `major-mode-hydra'
;; NOTE: Before using, must run jedi:install-server
;; and jedi:setup if the current buffer is visiting a python file
(use-package jedi
  :init
  ;; Uncomment next line if you like the menu right away
  (setq ac-show-menu-immediately-on-auto-complete t)
  ;; Can also express in terms of ac-delay var, e.g.:
  ;;   (setq ac-auto-show-menu (* ac-delay 2))
  :config
  ;; Don't let tooltip show up automatically
  (setq jedi:get-in-function-call-delay 10000000)
  ;; Start completion at method dot
  (setq jedi:complete-on-dot t)
  :bind
  (("M-." . jedi:goto-definition)
   ("M-," . jedi:goto-definition-pop-marker)
   ("M-?" . jedi:show-doc)
   ("M-/" . jedi:get-in-function-call))
  :hook
  ;; Enable Jedi setup on mode start
  (python-mode . jedi:setup))

(provide 'setup-auto-complete)
;;; setup-auto-complete.el ends here
