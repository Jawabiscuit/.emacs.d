;;; `setup-spell-check.el' --- Setup spell checker packages. -*- lexical-binding: t -*-
;;
;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`auto-complete')
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
(use-package flyspell
  :when (executable-find "hunspell")
  :init (when (executable-find "hunspell") (setq ispell-program-name "hunspell"))
  :bind (("C-c h s" . hydra-flyspell/body))
  :config
  (general-unbind
    :keymaps 'flyspell-mode-map
    :package 'flyspell
    "C-," "C-." "C-M-i" "C-c $" "C-;"))

;; Automatic dictionary selection based on buffer content
(use-package auto-dictionary)

(use-package ac-ispell
  :after auto-complete
  :config
  ;; Binary downloaded and placed in ~/.emacs.d/hunspell
  (setq ispell-complete-word-dict
    (concat (expand-file-name user-emacs-directory)
            "hunspell/share/hunspell/en_US.dic"))

  ;; Completion words longer than 4 characters
  (custom-set-variables
    '(ac-ispell-requires 4)
    '(ac-ispell-fuzzy-limit 2))
  
  (eval-after-load "auto-complete"
    '(progn
        (ac-ispell-setup)))
  
  (general-add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
  (general-add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
  (general-add-hook 'org-mode-hook 'ac-ispell-ac-setup)
  (general-add-hook 'text-mode-hook 'ac-ispell-ac-setup)

  (ac-ispell-ac-setup))

(provide 'setup-spell-check)
;;; setup-spell-check.el ends here
