(use-package dash
  :ensure t)

(require 'package)
(require 'dash)

;; Add melpa to package repos
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(setq package-pinned-packages '())

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;; On-demand installation of packages
;; Use use-package instead

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; More packages
(setq byte-compile--use-old-handlers nil) ; silence warning in emacs <25.1

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

(provide 'setup-package)
