;; TODO: most of this stuff can be replaced by use-package
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

;; Project management
(use-package projectile
  :ensure t)
(projectile-mode +1)

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

(provide 'setup-package)
