;;; setup-markdown-mode.el --- Setup Emacs Major mode for Markdown formatted files -*- lexical-binding: t -*-
;;
;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`pandoc')
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

(use-package markdown-mode
  :init
  (setq-default markdown-hide-markup t)
  (setq markdown-command "pandoc")
  (setq markdown-imenu-generic-expression
      '(("title"  "^\\(.*\\)[\n]=+$" 1)
        ("h2-"    "^\\(.*\\)[\n]-+$" 1)
        ("h1"     "^# \\(.*\\)$" 1)
        ("h2"     "^## \\(.*\\)$" 1)
        ("h3"     "^### \\(.*\\)$" 1)
        ("h4"     "^#### \\(.*\\)$" 1)
        ("h5"     "^##### \\(.*\\)$" 1)
        ("h6"     "^###### \\(.*\\)$" 1)
        ("fn"     "^\\[\\^\\(.*\\)\\]" 1)))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . pandoc-mode)
         (markdown-mode . (lambda ()
            (setq imenu-generic-expression
                  markdown-imenu-generic-expression)))))

(provide 'setup-markdown-mode)
;;; setup-markdown-mode.el ends here
