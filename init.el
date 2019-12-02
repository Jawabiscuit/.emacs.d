;; -*- no-byte-compile: t -*-
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; Configure straight.el
(load-file (expand-file-name "core/straight.el" user-emacs-directory))

(org-babel-load-file "~/.emacs.d/configuration.org")
