;;; setup-tabs --- Tab display setup -*- lexical-binding: t -*-

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
;; Custom defuns: akirak/emacs.d

;;; Code:

(use-package centaur-tabs
  :config
  (advice-add #'fit-window-to-buffer
              :before #'akirak/disable-centaur-tabs-before-fit-window-buffer)

  (defun akirak/disable-centaur-tabs-before-fit-window-buffer
      (&optional window &rest args)
    "Disable centaur-tabs in any buffers that are displayed using
    fit-window-to-buffer."
    (let ((buffer (if window
                      (window-buffer window)
                    (current-buffer))))
      (unless (local-variable-p 'centaur-tabs--local-hlf buffer)
        (with-current-buffer buffer
          (centaur-tabs-local-mode 1)))))

  (defun ceutaur-tabs-hide-tab (x)
    (or (string-match-p (rx (or "*helm"
                                "*direnv*"
                                "special*"
                                "*LV*"))
                        (format "%s" x))
        (derived-mode-p 'magit-process-mode)))

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    *Messages*, *Warnings*, and *Backtrace* buffers get grouped to \"Debug\"
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
			  help-mode))
       "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode))
       "Org")
      ((and (string-match-p (rx bol (or "*Messages*"
                                        "*Warnings*"
                                        "*Backtrace*"))
                            (buffer-name)))
       "Debug")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  (centaur-tabs-mode 1)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-close-button "X")
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  (centaur-tabs-cycle-scope 'tabs))

(provide 'setup-tabs)
;;; setup-tabs.el ends here
