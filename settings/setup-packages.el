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
(straight-use-package 'auctex)

; Diminished modes from modeline
(use-package diminish)

; Increase selected region by semantic units
(use-package expand-region)

; Convert buffer text and decorations to HTML.
(use-package htmlize)

; Complete Git interface
(use-package magit)

; Gitflow extension for magit
(use-package magit-gitflow)

; Multiple cursors for Emacs
(use-package multiple-cursors)

; Show bullets in org-mode as UTF-8 characters
(use-package org-bullets)

(use-package paredit)

; In editor rest server/client
(use-package restclient)

; Auto closure for parenthesis and other characters
(use-package smartparens)

; Smart M-x
(use-package smex)

; Make emacs scroll smoothly
(use-package smooth-scrolling)

; Various completion functions using Ivy, Swiper for search
(use-package counsel)

;; Emacs Major mode for Markdown-formatted files
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
)

;; Pandoc dependency
(use-package hydra)

;; Pandoc-mode is for interacting with pandoc, a program that converts
;; text files written in one markup language to another
(use-package pandoc-mode)

;; BEGIN Emacs iPython notebook (EIN) dependencies

;; string manipulation library
(use-package s)

;; live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode)

;; Compatible layer for URL request in Emacs
(use-package request)

;; Wrap request.el by deferred
(use-package request-deferred)

;; Emacs WebSocket client and server
(use-package websocket)

;; Python major mode
(use-package python-mode)

;; Support sequential operation which omits prefix keys
(use-package smartrep)

;; Multiple modes
(use-package polymode)

;; END EIN dependencies

;; Project management
(use-package projectile)

;; Fuzzy matching
(use-package flx-ido)

;; Standard auto-completion
(use-package auto-complete)

;; Middleware so python-land can communicate with emacs-land
(use-package epc)

;; Brains of Python auto-complete
(use-package jedi)

;; Mel mode dependencies
(use-package browse-url-dwim)

;; Git-gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode))

(provide 'setup-packages)
