;;; setup-directory.el --- Configurations for directory editing -*- lexical-binding: t -*-
;;
;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`use-package')
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

(require 'dired)

(jawa/bind-jump "d" 'dired-jump)

(setq-default dired-dwim-target t
              dired-recursive-deletes 'always
              dired-recursive-copies 'always
              dired-isearch-filenames t)

;; Formatting
(setq dired-listing-switches "-alh")

(use-package ls-lisp
  :straight nil
  :config
  (cond
   ((eq system-type 'windows-nt)
    (setq ls-lisp-verbosity nil))
   ((or (jawa/windows-subsystem-for-linux-p))
    (setq ls-lisp-verbosity '(links)))
   (t (setq ls-lisp-verbosity '(links uid gid))))
  :custom
  (ls-lisp-use-insert-directory-program nil))

;; File size format
(defun jawa/ls-lisp-format-file-size (file-size human-readable)
  (if human-readable
      (format " %6s" (file-size-human-readable file-size))
    (format (if (floatp file-size)
                ls-lisp-filesize-f-fmt
              ls-lisp-filesize-d-fmt)
            file-size)))

(advice-add #'ls-lisp-format-file-size :override
            #'jawa/ls-lisp-format-file-size)

;; Time format
(defun jawa/ls-lisp-format-time (file-attr time-index)
  (let ((time (nth (or time-index 5) file-attr)))
    (format-time-string "%F %R" time)))

(advice-add #'ls-lisp-format-time :override
            #'jawa/ls-lisp-format-time)

;; Enhancing Dired
(use-package dired-hide-dotfiles
  :commands (dired-hide-dotfiles-mode))

;; Many extensions of dired here
;; https://github.com/Fuco1/dired-hacks
(use-package dired-hacks-utils
  :general
  (:keymaps 'dired-mode-map
            "n" 'dired-hacks-next-file
            "p" 'dired-hacks-previous-file))

(use-package dired-open
  :custom
  (dired-open-functions '(dired-open-by-extension
                          dired-open-subdir)))

(use-package ivy-dired-history
  :init
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))

(use-package dired-filter
  :config
  (setq dired-filter-group-saved-groups
   `(("default"
      ("Directories"
       (directory . t))
      ("Dotfiles"
       (name . "^\\."))
      ;; Text files
      ("Text"
       (or (name . "README")
           (name . "TODO")
           (name . "LICENSE")
           (extension "txt" "md" "mkd" "markdown" "rst")))
      ("Org"
       (extension "org" "bib"))
      ("Data files"
       (extension "csv" "json" "sql"))
      ;; Binary files
      ("Books and papers"
       (extension "pdf" "mobi" "epub" "azw"))
      ("Archives"
       (extension "zip" "rar" "gz" "bz2" "tar"))
      ("Disk images"
       (extension "iso" "ova"))
      ("Office docs"
       (extension "xlsx" "xls" "docx" "doc"))
      ("Programs"
       (extension "exe" "run" "deb"))
      ("Objects and binary files"
       (extension "o" "elc"))
      ("Meta data"
       (extension "torrent" "acsm"))
      ;; Images are often thumbnails, so they should come
      ;; after other binary files
      ("Images and graphics"
       (extension "jpg" "jpeg" "png" "gif" "svg"))
      ;; Source code
      ("Config"
       (or (name . "Makefile")
           (name . "Dockerfile")
           (extension "yml" "yaml" "cabal"
                      "dockerfile" "mk")))
      ("Emacs Lisp"
       (extension "el")))))
  :hook
  (dired-mode . dired-filter-mode)
  (dired-mode . dired-filter-group-mode))

(use-package dired-subtree
  :init
  (bind-keys
   :map dired-mode-map
   :prefix "C-,"
   :prefix-map dired-subtree-map
   :prefix-docstring "Dired subtree map"
   ("RET" . dired-subtree-toggle)
   ("C-i" . dired-subtree-insert)
   ("C-/" . dired-subtree-apply-filter)
   ("C-k" . dired-subtree-remove)
   ("C-n" . dired-subtree-next-sibling)
   ("C-p" . dired-subtree-previous-sibling)
   ("C-j" . dired-subtree-up)
   ("C-k" . dired-subtree-down)
   ("C-b" . dired-subtree-beginning)
   ("C-e" . dired-subtree-end)
   ("C-m" . dired-subtree-mark-subtree)
   ("C-u" . dired-subtree-unmark-subtree)
   ("C-o C-f" . dired-subtree-only-this-file)
   ("C-o C-d" . dired-subtree-only-this-directory)))

(provide 'setup-directory)
;;; setup-directory.el ends here
