;;; setup-org-starter.el --- Setup for org-starter  -*- lexical-binding: t -*-
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

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list org-directory))

(use-package org-reverse-datetree)

(use-package org-ql-search
  :straight org-ql)

(use-package org-ql-view
  :straight org-ql)

;; `org-starter-path': "Load path" for Org files
(unless (bound-and-true-p org-starter-path)
  (setq org-starter-path `(,(abbreviate-file-name
                             (expand-file-name
                              "org-starter"
                              no-littering-etc-directory)))))

(use-package org-starter
  :config
  (org-starter-mode 1)
  (jawa/bind-user "t" 'org-starter-find-file-by-key)
  (org-starter-def org-default-notes-file
    :key "n"
    :refile (:maxlevel . 5))
  (org-starter-def (concat org-directory "/todo.org")
    :key "t"
    :refile (:maxlevel . 3))
  (org-starter-def (expand-file-name "configuration.org" user-emacs-directory)
    :key "c"
    :refile (:maxlevel . 3))
  (general-add-hook 'org-starter-extra-find-file-map
                    '((";" org-starter-find-config-file "config")
                      ;; ("w" org-plain-wiki "wiki")
                      )
                     t)
  (general-add-hook 'org-starter-extra-alternative-find-file-map
                    '((";" org-starter-swiper-config-files "config")
                      ;; ("w" helm-org-rifle-wiki "wiki/writing")
                      )
                     t)
  (general-add-hook 'org-starter-extra-refile-map
                    '(("'" avy-org-refile-as-child "avy")
                      ("?" akirak/org-refile-same-buffer "same buffer"))
                     t)
  :custom
  ;; Define where org files are kept
  (org-starter-define-directory org-directory)

  ;; Enable external configuration files loaded from `org-starter-path'
  (org-starter-load-config-files t)
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))

  ;; (org-starter-alternative-find-file-command #'helm-org-rifle-files)
  (org-starter-find-file-visit-window t)
  (org-starter-override-agenda-window-setup 'other-window)
  (org-starter-enable-local-variables :all))

;; Use `org-starter' with swiper
(use-package org-starter-swiper)

(use-package org-starter-extras
  :straight (org-starter-extras :host github :repo "akirak/org-starter"
                                :files ("org-starter-extras.el")))

(provide 'setup-org-starter)
;;; setup-org-starter.el ends here
