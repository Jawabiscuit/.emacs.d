;; Rather than grouping init files by subdirectories,
;; I have decided to put all of them in a single directory,
;; like Kaushal Modi does.
;; https://github.com/kaushalmodi/.emacs.d

;; The new setup files should be prefixed with setup-, and
;; the directory is named setup save typing.

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))

(defvar jawa/settings-failed-modules nil
  "List of modules failed to load.")

(defvar jawa/settings-module-worked-on nil)

(defun jawa/settings-find-failed-module ()
  "Open a module which has been failed to load."
  (interactive)
  (if-let* ((feature (setq jawa/settings-module-worked-on
                           (or jawa/settings-module-worked-on
                               (pop jawa/settings-failed-modules))))
            (filename (expand-file-name (concat "settings/" (symbol-name feature) ".el")
                                         user-emacs-directory)))
      (when (file-exists-p filename)
        (find-file filename)
        (display-buffer "*Backtrace*"))
    (message "No broken module left")))

(defcustom jawa/blacklisted-features nil
  "List of features to prevent loading.
  This is applicable for modules loaded by `jawa/settings-load' and `jawa/require'."
  :type '(repeat symbol)
  :group 'jawa)

(cl-defun jawa/settings-load (feature &optional severity
                                     &key (when t))
  "Load a configuration module.
  FEATURE should be a module in `~/.emacs.d/settings'.
  If SEVERITY is non-nil, abort the initialization process."
  (when (and when
             (if (member feature jawa/blacklisted-features)
                 (progn
                   (message "Module %s is blacklisted. See `jawa/blacklisted-features'" feature)
                   nil)
               t)
             (not (require feature nil t)))
    (add-to-list 'jawa/settings-failed-modules feature t)
    (message "Failed to load %s" feature)
    (when severity
      (jawa/settings-find-failed-module)
      (error "Aborted due to a failed module."))))

(defalias 'jawa/require 'jawa/settings-load)
