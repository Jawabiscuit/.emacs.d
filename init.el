;; -*- no-byte-compile: t -*-
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; @akirak
;; Expand the GC threshold until gcmh-mode is activated.
;; gcmh-mode updates this value later, so you don't have to reset it.
;; The value is stolen from http://akrl.sdf.org/
(setq gc-cons-threshold #x40000000)

;; Configure straight.el
(load-file (expand-file-name "core/straight.el" user-emacs-directory))

;; Diminished modes from modeline
(use-package diminish)

;; Use the latest Git version of Org mode
(require 'cl-lib)
(require 'subr-x)

;; Remove org-mode shipped with Emacs from load-path
(cl-delete-if (lambda (dpath) (string-match-p "/org/?$" dpath)) load-path)

;; Install org-mode from the Git repository
(load-file (expand-file-name "core/org-from-git.el" user-emacs-directory))

;; Load configuration files
(load-file (expand-file-name "core/setup.el" user-emacs-directory))

;; Prevent a confirmation dialog when the org file is loaded.
;; Don't forget to revert this variable at the beginning of the Org file.
(setq-default enable-local-variables :all)

(org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))
