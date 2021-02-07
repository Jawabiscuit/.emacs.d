;; -*- no-byte-compile: t -*-

;; Debug startup
;; (setq debug-on-error t)
;; To make debug on error persist after init
;; (add-hook 'after-init-hook
;;           '(lambda () (setq debug-on-error t)))

;; Emacs version requirement
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; Expand the GC threshold until gcmh-mode is activated.
;; gcmh-mode updates this value later, so you don't have to reset it.
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 1000 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'cl-lib)
;; Load configuration files
(load-file (expand-file-name "core/setup.el" user-emacs-directory))

;; Configure straight.el
(load-file (expand-file-name "core/straight.el" user-emacs-directory))

;; Diminished modes from modeline
(use-package diminish)

;; Garbage collection magic hack (after init)
(use-package gcmh
  :straight (gcmh :type git :flavor melpa :host gitlab :repo "koral/gcmh")
  :diminish gcmh-mode
  :hook (after-init . gcmh-mode)
  ;; :config
  ;; (gcmh-mode 1)
  :custom
  (gcmh-verbose nil)
  (gcmh-idle-delay 15))

;; Use the latest Git version of Org mode
(require 'subr-x)

;; Remove org-mode shipped with Emacs from load-path
(cl-delete-if (lambda (dpath) (string-match-p "/org/?$" dpath)) load-path)

;; Install org-mode from the Git repository
(load-file (expand-file-name "core/org-from-git.el" user-emacs-directory))

;; Prevent a confirmation dialog when the org file is loaded.
;; Don't forget to revert this variable at the beginning of the Org file.
(setq-default enable-local-variables :all)

;; Load the main config
(org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))
