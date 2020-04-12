;;; setup-mel --- Mel in emacs -*- lexical-binding: t -*-

;; Author: Jonas Avrin
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

;; mel outline mode
(require 'mel-magic)

(use-package mel-magic
  :load-path site-lisp-dir
  :straight (mel-magic :type built-in)
  :config
  (add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
  (autoload 'mel-mode "mel-mode" nil t))

(provide 'setup-mel)
;;; setup-mel.el ends here
