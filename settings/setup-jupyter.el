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

;; System constants
(require 'system-const)

;; live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode)

;; Compatible layer for URL request in Emacs
(use-package request)

;; Wrap request.el by deferred
(use-package request-deferred)

;; Emacs WebSocket client and server
(use-package websocket)

;; Support sequential operation which omits prefix keys
(use-package smartrep)

;; Multiple modes
(use-package polymode)

;; Emacs iPython notebook
(use-package ein
  ;; Apparently ob-ipython is better, and I like org-mode so...
  ;; I've found working with `ein' pretty clunky.
  :disabled t
  :init
  ;; Omit a bunch of key chord prefix typing
  (setq ein:use-smartrep t)
  ;; Use jedi autocomplete backend
  (setq ein:completion-backend 'ein:use-ac-jedi-backend)
  :config
  ;; Execute ein source blocks in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((ein . t))))
  )

;; Allow org-mode to evaluate code blocks using a Jupyter kernel
;; Issues with loading ob-ipython on bss machine
(if (not (or (and sys/bss-hostname-p (executable-find "jupyter"))
         (and sys/home-hostname-p (not (executable-find "jupyter")))))
    (use-package ob-ipython
      :straight (ob-ipython
                 :type git
                 :flavor melpa
                 :files (:defaults "*.py" "ob-ipython-pkg.el")
                 :host github
                 :repo "gregsexton/ob-ipython")
      :config
      (message "Found jupyter. Loaded ob-ipython.")
      (org-babel-do-load-languages
       'org-babel-load-languages
       (append org-babel-load-languages '((ipython . t)))))
    (if sys/bss-hostname-p
        (message "This host is prevented from loading ob-ipython."))
    (message "ob-ipython package was not loaded."))

 (provide 'setup-jupyter)
;;; setup-jupyter.el ends here
