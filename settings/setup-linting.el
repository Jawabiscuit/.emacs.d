;;; `setup-linting.el' --- Setup code linting. -*- lexical-binding: t -*-
;;
;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`avy')
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
;;
;; On-the-fly syntax checking
(use-package flycheck-package
  :bind (("C-c h c" . hydra-flycheck/body))
  :commands (flycheck-package-setup))

;; Jump to and fix syntax errors using flycheck with avy interface
(use-package avy-flycheck
  :after avy
  :commands (avy-flycheck-setup))

(use-package flycheck-indicator
  :after flycheck)

;; Shows error under point in pos-tip popups
(use-package flycheck-pos-tip
  :after flycheck-package
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;; Code formatter
(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  ;; :config
  ;; (apheleia-global-mode +1)
)

(provide 'setup-linting)
;;; setup-linting.el ends here
