
;; Set exec path from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
     (exec-path-from-shell-initialize)))

;; LaTeX
(straight-use-package 'auctex)

;; Diminished modes from modeline
(use-package diminish)

;; Project management
(use-package projectile)

;; A generic completion mechanism
(jawa/require 'setup-ivy)

;; Popup frame
(use-package posframe)

;; Icons everywhere
(jawa/require 'setup-all-the-icons)

;; Tie related commands into a family of short bindings with a prefix
(jawa/require 'setup-hydra)

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
(jawa/require 'setup-smartparens)

;; Smart M-x (Deactivated)
(use-package smex
  :disabled t
  :config
  (smex-initialize))

;; Make emacs scroll smoothly
(use-package smooth-scrolling)

;; Emacs Major mode for Markdown-formatted files
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
)

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

;; Fuzzy matching
(use-package flx-ido)

;; Standard auto-completion
(use-package auto-complete)

;; Middleware so python-land can communicate with emacs-land
(use-package epc)

;; Brains of Python auto-complete
;; IMPORTANT: Jedi not fully compatible with `major-mode-hydra'
(use-package jedi
  :init
  ;; Uncomment next line if you like the menu right away
  (setq ac-show-menu-immediately-on-auto-complete t)
  ;; Can also express in terms of ac-delay var, e.g.:
  ;;   (setq ac-auto-show-menu (* ac-delay 2))
  ;; Enable Jedi setup on mode start
  (general-add-hook 'python-mode-hook 'jedi:setup)
  :config
  ;; Don't let tooltip show up automatically
  (setq jedi:get-in-function-call-delay 10000000)
  ;; Start completion at method dot
  (setq jedi:complete-on-dot t)
  :bind
  (("M-." . jedi:goto-definition)
   ("M-," . jedi:goto-definition-pop-marker)
   ("M-?" . jedi:show-doc)
   ("M-/" . jedi:get-in-function-call))
)

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
;; (use-package mini-modeline
;;   :init
;;   (setq mini-modeline-enhance-visual nil)
;;   :straight (mini-modeline :host github :repo "kiennq/emacs-mini-modeline")
;;   :after smart-mode-line
;;   :config
;;   (setq mini-modeline-echo-duration 8)
;;   (setq mini-modeline--echo-keystrokes t)
;;   (mini-modeline-mode t))

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
