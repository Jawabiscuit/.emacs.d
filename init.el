;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable installation of packages from MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and
  ;; MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Site-lisp
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Settings
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Functions
(setq defuns-dir
      (expand-file-name "defuns" user-emacs-directory))

;; (load all files in defuns-dir)
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Setup load-path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Setup packages
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
;; TODO: replace stuff in here with use-package
(require 'setup-package)

;; Keep emacs Custom-settings in separate file
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     diminish
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
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(require 'setup-smartparens)
(require 'multiple-cursors)

;; Language specific setup files
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Map files to modes
(require 'mode-mappings)

;; Setup key bindings
(require 'key-bindings)

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Buffer switching
(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

;; Editing (some definitions are in editing_defuns.el)
(add-hook 'text-mode-hook 'casey-big-fun-text-hook)

;; Window
(add-hook 'window-setup-hook 'post-load-stuff t)
