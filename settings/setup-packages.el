;;; `setup-packages.el' --- Summary: Main packages
;;; Commentary:
;; `use-package' blocks should be tidy, or else converted to a setup file
;;; Code:

;; string manipulation library
(use-package s)

;; Try a package without installing it
(use-package try
  :commands try)

;; Set exec path from shell - for Unix-y terminals
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;; History, recent files
(jawa/require 'setup-history)

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

;; Project management
(jawa/require 'setup-projects)

;; Persistant window configurations
(jawa/require 'setup-persp)

;; Icons everywhere
(jawa/require 'setup-all-the-icons)

;; Tie related commands into a family of short bindings with a prefix
(jawa/require 'setup-hydra)

;; Rad dashboard
(jawa/require 'setup-dashboard)

;; Tabbed interface
;; (jawa/require 'setup-tabs)

;; Grepping
(jawa/require 'setup-search)

;; Completion engine
;; (jawa/require 'setup-jedi) ; councel, elpy

;; Standard auto-completion
(jawa/require 'setup-auto-complete)

;; Spell checker
(jawa/require 'setup-spell-check)

;; Integrate emacs with powerthesaurus.org
(use-package powerthesaurus
  :defer t)

;; On-the-fly syntax checking
(jawa/require 'setup-linting)

;; Popular method to navigate and edit Lisp code
;; (jawa/require 'setup-elisp)

;; Graphically indicate the fill column
(use-package fill-column-indicator
  :defer t)

;; LaTeX
(when (executable-find "auctex")
     (use-package auctex
       :defer t
       :straight
       (auctex
        :type git
        :host github
        :repo "emacs-straight/auctex")))

;; Window switching convenience
(use-package switch-window
  :defer t
  :straight (switch-window :host github :repo "dimitri/switch-window")
  :bind ("C-c h w" . hydra-windows/body))

;; More window switching
(use-package ace-window
  :defer t)

;; Popup window management
(use-package shackle
  :disabled t
  :config
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  ;; provides a less intrusive user experience to select all windows
  ;; by default unless they are spawned by compilation-mode and
  ;; demonstrates how to use exceptions
  (setq shackle-rules '((compilation-mode :noselect t)
                        ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4))
        shackle-default-rule '(:select t)))

;; Increase selected region by semantic units
(use-package expand-region
  :defer t)

;; Convert buffer text and decorations to HTML.
(use-package htmlize
  :defer t)

;; Complete Git interface
(jawa/require 'setup-magit)

;; Multiple cursors for Emacs
(use-package multiple-cursors
  :bind (("C-c h m" . hydra-mc/body)
         :map mc/keymap
         ("<return>" . nil)))

(use-package mc-extras
  :after multiple-cursors
  :straight (mc-extras :host github :repo "knu/mc-extras.el"))

(use-package paredit
  :defer t)

;; In editor rest server/client
(use-package restclient
  :defer t)

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

;; Extensions to outline mode
(use-package outline-magic
  :bind (:map outline-minor-mode
	      ("<C-tab>" . outline-cycle)))

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

;; Docker highlighting
(use-package dockerfile-mode
  :straight (dockerfile-mode :host github :repo "spotify/dockerfile-mode")
  :mode "\\Dockerfile\\'"
  ;; Change the binary name
  ;; :config (setq dockerfile-mode-command "docker")
)

;; Docker tramp
(use-package docker-tramp
  :straight (docker-tramp
             :type git
             :flavor melpa
             :host github
             :repo "emacs-pe/docker-tramp.el")
  :defer t)

;; auto-fill without editing
(use-package virtual-auto-fill
  :straight (virtual-auto-fill
             :type git
             :host github
             :repo "luisgerhorst/virtual-auto-fill"))

;; Visualize undos
(use-package undo-tree
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode))

;; Emacs wide font scale
(use-package default-text-scale
  :straight (:type git
                   :flavor melpa
                   :host github
                   :repo "purcell/default-text-scale")
  :config
  (default-text-scale-mode +1))

;; Jira
(jawa/require 'setup-jira)

;; Org-mode
(jawa/require 'setup-org t)

(provide 'setup-packages)
;;; setup-packages.el ends here
