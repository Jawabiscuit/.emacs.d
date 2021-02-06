;; https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package using straight.el
(straight-use-package 'use-package)

;; Use straight.el by default in use-package directives
(setq straight-use-package-by-default t)

;; `use-package' statistics. Run `use-package-report' before closing.
(setq use-package-compute-statistics t)

(use-package el-patch
  :custom
  (el-patch-enable-use-package-integration t))

;; Temporary variable to set for optimizing startup
;; (setq use-package-verbose t)
