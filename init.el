;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(package-initialize)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Setup load path
(setq handmade-dir "W:/handmade/")
(setq settings-dir
      (concat handmade-dir
	      (convert-standard-filename ".emacs.d/settings")))
(add-to-list 'load-path settings-dir)

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

