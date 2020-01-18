;;; setup-jupyter --- Jupyter notebooks in Emacs -*- lexical-binding: t -*-

;; Author: emacser
;; URL: https://www.github.com/jawabiscuit
;; Package-Requires: (`')

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

(use-package ein
  :init
  ;; Omit a bunch of key chord prefix typing
  (setq ein:use-smartrep t)
  ;; Use jedi autocomplete backend
  (setq ein:completion-backend 'ein:use-ac-jedi-backend)
  :config
  ;; Execute ein source blocks in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ein . t)
     )))

(provide 'setup-jupyter)
;;; setup-jupyter.el ends here
