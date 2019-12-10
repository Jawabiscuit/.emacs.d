
;; Ivy
(use-package ivy
  :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (ivy-mode 1)
)

;; LaTeX
(straight-use-package 'auctex)

;; Diminished modes from modeline
(use-package diminish)

;; Increase selected region by semantic units
(use-package expand-region)

;; Convert buffer text and decorations to HTML.
(use-package htmlize)

;; Complete Git interface
(use-package magit
  :config
  (jawa/require 'setup-magit))

;; Gitflow extension for magit
(use-package magit-gitflow
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; Multiple cursors for Emacs
(use-package multiple-cursors)

(use-package paredit)

;; In editor rest server/client
(use-package restclient)

;; Auto closure for parenthesis and other characters
(use-package smartparens
  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode)
  (add-hook 'js-mode-hook 'smartparens-mode)
  
  ;; elisp comment highlighting
  ;; Redefine this global pair (`LaTex') to a new value locally
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (sp-local-pair 'org-mode "=" "=" :wrap "C-+")
  
  ;; New pair
  (sp-pair "<" ">")
  
  ;; Local pairs can be removed by calling `sp-local-pair' with optional keyword argument `:actions' with value `:rem'
  ;; (sp-local-pair 'LaTeX-mode "`" nil :actions :rem)
  
  ;; Create a wrapping
  ;; Usage
  ;;
  ;; |foobar
  ;; hit C-(
  ;; becomes (|foobar)
  (sp-pair "(" ")" :wrap "C-("))

;; Smart M-x
(use-package smex
  :config
  (smex-initialize))

;; Make emacs scroll smoothly
(use-package smooth-scrolling)

;; Various completion functions using Ivy, Swiper for search
(use-package counsel
  :config
  (counsel-mode 1))

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

;; Sexy modeline
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  :config
  ;; Bypass Emacs' loading themes warning
  (sml/setup)
  (setq sml/theme 'dark)
)

;; Hide the mode-line and display its information in minibuffer
(use-package mini-modeline
  :init
  (setq mini-modeline-enhance-visual nil)
  :straight (mini-modeline :host github :repo "kiennq/emacs-mini-modeline")
  :after smart-mode-line
  :config
  (setq mini-modeline-echo-duration 8)
  (setq mini-modeline--echo-keystrokes t)
  (mini-modeline-mode t))

;; Yasnippet
(jawa/require 'setup-yasnippet)

;; Avy
(jawa/require 'setup-avy)

;; Yankpad
(use-package yankpad
  :config
  (jawa/bind-register
    "M-y" #'yankpad-repeat)
  (jawa/require 'setup-yankpad))

(provide 'setup-packages)
