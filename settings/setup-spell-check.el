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

;; (when (executable-find "hunspell")
;;    (setq ispell-alternate-dictionary "/usr/share/hunspell/en_US.dic"
;;          ispell-local-dictionary-alist
;;          '(("english"
;;             "[[:alpha:]]"
;;             "[^[:alpha:]]"
;;             "[']"
;;             t
;;             ("-d" "en_US" "-p" "/usr/share/hunspell")
;;             nil
;;             iso-8859-1))
;;          ispell-dictionary "english"))

(when (executable-find "hunspell")
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US")
  (when sys/wslp (setq ispell-alternate-dictionary "/usr/share/hunspell/en_US.dic"
                       ispell-local-dictionary-alist
                       '(("english"
                          "[[:alpha:]]"
                          "[^[:alpha:]]"
                          "[']"
                          t
                          ("-d" "en_US" "-p" "/usr/share/hunspell")
                          nil
                          iso-8859-1))
                       ispell-dictionary "english"))
  (when sys/win32p (setq ispell-program-name "hunspell"
                         ispell-complete-word-dict
                         (concat (expand-file-name user-emacs-directory)
                                 "hunspell/share/hunspell/en_US.dic"))))

(use-package flyspell
  :when (executable-find "hunspell")
  :bind (("C-c h s" . hydra-flyspell/body))
  :config
  (general-unbind
    :keymaps 'flyspell-mode-map
    :package 'flyspell
    "C-," "C-." "C-M-i" "C-c $" "C-;"))

(use-package ac-ispell
  :when (executable-find "hunspell")
  :after auto-complete
  :config
  ;; Completion words longer than 3 characters
  (custom-set-variables
   '(ac-ispell-requires 3)
   '(ac-ispell-fuzzy-limit 3))
  
  (eval-after-load "auto-complete"
    '(progn
       (ac-ispell-setup)))
  
  ;; (ac-ispell-ac-setup)
  :hook ((git-commit-mode . ac-ispell-ac-setup)
         (mail-mode . ac-ispell-ac-setup)
         (org-mode . ac-ispell-ac-setup)
         (text-mode . ac-ispell-ac-setup)))

(provide 'setup-spell-check)
;;; setup-spell-check.el ends here
