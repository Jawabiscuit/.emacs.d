;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(package-initialize)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq handmade-dir "w:/handmade/")
(setq emacsd-dir
      (concat handmade-dir
	      (convert-standard-filename ".emacs.d/")))

;; Site-lisp
(setq site-lisp-dir
      (concat emacsd-dir
	      (convert-standard-filename "site-lisp/")))

;; Diminish
(setq diminish-dir
      (concat site-lisp-dir
	      (convert-standard-filename "diminish")))

;; Settings
(setq settings-dir
      (concat emacsd-dir
	      (convert-standard-filename "settings")))

;; Setup load-path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path diminish-dir)

;; Setup packages
(require 'setup-package)

;; Keep emacs Custom-settings in separate file
(setq custom-file
      (concat handmade-dir
	      (convert-standard-filename ".emacs.d/custom.el")))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     magit
     magit-gitflow
     markdown-mode
     multiple-cursors
     paredit
     restclient
     smartparens
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Setup extensions
(eval-after-load 'magit '(require 'setup-magit))
(require 'setup-smartparens)

;; Language specific setup files
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Map files to modes
(require 'mode-mappings)

;; Setup key bindings
(require 'key-bindings)

