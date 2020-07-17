;;; `org-defuns' --- Summary: Global org functions
;;; Commentary:
;;; Code:

;; Tramp functions
(defun tramp-extended-path (&optional extended-path)
  "Extend TRAMP-REMOTE-PATH with EXTENDED-PATH."
  (if extended-path
      (format "%s/%s" (getenv "TRAMP-REMOTE-PATH") extended-path)
    (format "%s" (getenv "TRAMP-REMOTE-PATH"))))

(defun setenv-tramp-remote-path (&optional path)
  "Set TRAMP-REMOTE-PATH env variable to PATH."
  (if path
      (setenv "TRAMP-REMOTE-PATH" path)))

(defun setenv-do-oath-access-token (&optional value)
  "Set DIGITALOCEAN_ACCESS_TOKEN env variable to VALUE."
  (if value
      (setenv "DIGITALOCEAN_ACCESS_TOKEN" value)))
