;; Window Commands
;;; `window-defuns' --- Summary: Interacting with Windows
;;; Requires: `system-const.el'
;;; Commentary:
;;; Code:
(require 'system-const)

(defun maximize-frame ()
    "Maximize the current frame"
    (interactive)
     (when sys/win32p (w32-send-sys-command 61488)))

(defun minimize-frame ()
  "Minimize the current frame"
  (interactive)
  (when sys/win32p (w32-send-sys-command 61472)))

(defun restore-frame ()
    "Restore a minimized frame"
     (interactive)
     (when sys/win32p (w32-send-sys-command 61728)))
