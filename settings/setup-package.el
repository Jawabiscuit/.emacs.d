(require 'use-package)

;; Add melpa to package repos
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Ivy
(use-package ivy
  :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
)

;; LaTeX
;; `2019-12-01': Fresh install of .emacs.d
;; Had to download the zip from elpa and place it in `.emacs.d/elpa/' manually
(use-package tex
  :ensure auctex)

; Diminished modes from modeline
(use-package diminish
  :ensure t)

; Increase selected region by semantic units
(use-package expand-region
  :ensure t)

; Convert buffer text and decorations to HTML.
(use-package htmlize
  :ensure t)

; Complete Git interface
(use-package magit
  :ensure t)

; Gitflow extension for magit
(use-package magit-gitflow
  :ensure t)

; Multiple cursors for Emacs
(use-package org-bullets
  :ensure t)

; Show bullets in org-mode as UTF-8 characters
(use-package paredit
  :ensure t)

; In editor rest server/client
(use-package restclient
  :ensure t)

; Auto closure for parenthesis and other characters
(use-package smartparens
  :ensure t)

; Smart M-x
(use-package smex
  :ensure t)

; Make emacs scroll smoothly
(use-package smooth-scrolling
  :ensure t)

; Various completion functions using Ivy, Swiper for search
(use-package counsel
  :ensure t)

;; Emacs Major mode for Markdown-formatted files
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
)

;; Pandoc dependency
(use-package hydra
  :ensure t
)

;; Pandoc-mode is for interacting with pandoc, a program that converts
;; text files written in one markup language to another
(use-package pandoc-mode
  :ensure t
)

;; Emacs iPython notebook (EIN) dependencies
;; string manipulation library
(use-package s
  :ensure t)
;; live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :ensure t)
;; Compatible layer for URL request in Emacs
(use-package request
  :ensure t)
;; Wrap request.el by deferred
(use-package request-deferred
  :ensure t)
;; Emacs WebSocket client and server
(use-package websocket
  :ensure t)
;; Python major mode
(use-package python-mode
  :ensure t)
;; Support sequential operation which omits prefix keys
(use-package smartrep
  :ensure t)
(use-package polymode
  :ensure t)

;; Project management
(use-package projectile
  :ensure t)

;; Fuzzy matching
(use-package flx-ido
  :ensure t)

;; Standard auto-completion
(use-package auto-complete
  :ensure t)

;; Middleware so python-land can communicate with emacs-land
(use-package epc
  :ensure t)

;; Brains of Python auto-complete
(use-package jedi
  :ensure t)

;; Mel mode dependencies
(use-package browse-url-dwim
  :ensure t)

;; Git-gutter
(use-package git-gutter
  :ensure t)

;; yasnippet
(use-package yasnippet
  :ensure t)

;; yankpad
(use-package yankpad
  :ensure t)

(provide 'setup-package)
