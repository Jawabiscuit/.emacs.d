;;; `setup-packages.el' --- Where most packages are installed. -*- lexical-binding: t -*-

;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`use-package')
;; Homepage:
;; Keywords:


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `use-package' blocks should be tidy, or else converted to a setup file

;;; Code:

;; Try a package without installing it
(use-package try)

;; Set exec path from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; string manipulation library
(use-package s)

;; Tree plugin like NerdTree for Vim
(use-package neotree
  :bind ("<f8>" . neotree-toggle))

;; Doom modeline
(jawa/require 'setup-modeline)

;; Directory editing and tree view
(jawa/require 'setup-directory)

;; Flexible text folding
(use-package origami
  :bind (("C-c h f" . hydra-origami/body)))

;; Popup frame
(use-package posframe
;; Issues on Linux
  :disabled t)

;; A generic completion mechanism
(jawa/require 'setup-ivy)

;; Jump around windows using character keys
(jawa/require 'setup-avy)

;; Persistant window configurations
(jawa/require 'setup-persp)

;; Project management
(jawa/require 'setup-projects)

;; Icons everywhere
(jawa/require 'setup-all-the-icons)

;; History
(jawa/require 'setup-history)

;; Tie related commands into a family of short bindings with a prefix
(jawa/require 'setup-hydra)

;; Rad dashboard
(jawa/require 'setup-dashboard)

;; Tabbed interface
;; (jawa/require 'setup-tabs)

;; Grepping
(jawa/require 'setup-search)

;; Standard auto-completion
(jawa/require 'setup-auto-complete)

;; Spell checker
(jawa/require 'setup-spell-check)

;; Integrate emacs with powerthesaurus.org
(use-package powerthesaurus)

;; On-the-fly syntax checking
(jawa/require 'setup-linting)

;; Popular method to navigate and edit Lisp code
(jawa/require 'setup-elisp)

;; Graphically indicate the fill column
(use-package fill-column-indicator)

;; LaTeX
(straight-use-package 'auctex)

;; Window switching convenience
(use-package switch-window
  :straight (switch-window :host github :repo "dimitri/switch-window")
  :bind ("C-c h w" . hydra-windows/body))

;; More window switching
(use-package ace-window)

;; Popup window management
(use-package shackle
  :init
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  ;; provides a less intrusive user experience to select all windows
  ;; by default unless they are spawned by compilation-mode and
  ;; demonstrates how to use exceptions
  (setq shackle-rules '((compilation-mode :noselect t)
                        ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4))
        shackle-default-rule '(:select t)))

;; Increase selected region by semantic units
(use-package expand-region)

;; Convert buffer text and decorations to HTML.
(use-package htmlize)

;; Complete Git interface
(jawa/require 'setup-magit)

;; Multiple cursors for Emacs
(use-package multiple-cursors
  :bind ("C-c h m" . hydra-mc/body))
(use-package mc-extras
  :straight (mc-extras :host github :repo "knu/mc-extras.el"))

(use-package paredit)

;; In editor rest server/client
(use-package restclient)

;; Auto closure for parenthesis and other characters
(jawa/require 'setup-delimiters)

;; Smart M-x (Deactivated)
(use-package smex
  :disabled t
  :config
  (smex-initialize))

;; Fuzzy matching
(use-package flx-ido
  :disabled t)

;; Emacs Major mode for Markdown-formatted files
(jawa/require 'setup-markdown-mode)

;; Pandoc-mode is for interacting with pandoc, a program that converts
;; text files written in one markup language to another
(use-package pandoc-mode)

;; Mel mode dependencies
(use-package browse-url-dwim)

;; Mel
(jawa/require 'setup-mel)

;; Python
(jawa/require 'setup-python)

;; Jupyter notebooks in Emacs
(jawa/require 'setup-jupyter)

;; Yasnippet
(jawa/require 'setup-yasnippet)

;; Yankpad
(use-package yankpad
  :bind ("C-c h y" . hydra-yankpad/body)
  :config
  (jawa/bind-register "M-y" #'yankpad-repeat))

;; Log keyboard commands to buffer
(use-package command-log-mode)

(use-package dockerfile-mode
  :straight (dockerfile-mode :host github :repo "spotify/dockerfile-mode"))

;; Visualize undos
(use-package undo-tree
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode))

(provide 'setup-packages)
;;; setup-packages.el ends here
