;; Global file function definitions

;; Window Commands
(defun w32-restore-frame ()
    "Restore a minimized frame"
     (interactive)
     (w32-send-sys-command 61728)
)

(defun maximize-frame ()
    "Maximize the current frame"
     (interactive)
     (when (featurep 'aquamacs) (aquamacs-toggle-full-frame))
     (when (or (not(featurep 'aquamacs)) (not(featurep 'x))) (w32-send-sys-command 61488))
)

;; Time of day
(defun insert-timeofday ()
   (interactive "*")
   (insert (format-time-string "** Log <%a, %d %b %y: %I:%M%p>"))
)

;; Todo loading
(defun load-todo ()
  (interactive)
  (find-file casey-todo-file)
)

;; Log loading
(defun load-log ()
  (interactive)
  (find-file casey-log-file)
  (end-of-buffer)
  (newline)
  (insert-timeofday)
  (newline)
  (newline)
  (end-of-buffer)
)

;; Save buffer
(defun casey-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer)
)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name))))))
)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename))))
)

(defun copy-current-file-path ()
  "TODO: kill-new: Symbol’s value as variable is void: eproject-mode
   Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new (if eproject-mode
                  (s-chop-prefix (eproject-root) filename)
                filename)))
)

(defun find-or-create-file-at-point ()
  "Guesses what parts of the buffer under point is a file name and opens it."
  (interactive)
  (find-file (file-name-at-point))
)

(defun find-or-create-file-at-point-other-window ()
  "Guesses what parts of the buffer under point is a file name and opens it."
  (interactive)
  (find-file-other-window (file-name-at-point))
)

(defun file-name-at-point ()
  (save-excursion
    (let* ((file-name-regexp "[./a-zA-Z0-9\-_~]")
           (start (progn
                    (while (looking-back file-name-regexp)
                      (forward-char -1))
                    (point)))
           (end (progn
                  (while (looking-at file-name-regexp)
                    (forward-char 1))
                  (point))))
      (buffer-substring start end)))
)

(defun touch-buffer-file ()
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer)
)

(defun casey-header-format ()
   "Format the given file as a header file."
   (interactive)
   (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
   (insert "#if !defined(")
   (push-mark)
   (insert BaseFileName)
   (upcase-region (mark) (point))
   (pop-mark)
   (insert "_H)\n")
   (insert "/* ========================================================================\n")
   (insert "   $File: $\n")
   (insert "   $Date: $\n")
   (insert "   $Revision: $\n")
   (insert "   $Creator: Jonas Avrin $\n")
   (insert "   $Notice: (C) Copyright 2019. All Rights Reserved. $\n")
   (insert "   ======================================================================== */\n")
   (insert "\n")
   (insert "#define ")
   (push-mark)
   (insert BaseFileName)
   (upcase-region (mark) (point))
   (pop-mark)
   (insert "_H\n")
   (insert "#endif")
)

(defun casey-source-format ()
   "Format the given file as a source file."
   (interactive)
   (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
   (insert "/* ========================================================================\n")
   (insert "   $File: $\n")
   (insert "   $Date: $\n")
   (insert "   $Revision: $\n")
   (insert "   $Creator: Jonas Avrin $\n")
   (insert "   $Notice: (C) Copyright 2019. All Rights Reserved. $\n")
   (insert "   ======================================================================== */\n")
)

(defun casey-find-corresponding-file ()
  "(DEPRECATED: USE PROJECTILE CMD. Find the file that corresponds to this one."
  (interactive)
  (setq CorrespondingFileName nil)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.c" buffer-file-name)
     (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.h" buffer-file-name)
     (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
  (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
  (if (string-match "\\.hin" buffer-file-name)
     (setq CorrespondingFileName (concat BaseFileName ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
     (setq CorrespondingFileName (concat BaseFileName ".hin")))
  (if (string-match "\\.cpp" buffer-file-name)
     (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if CorrespondingFileName (find-file CorrespondingFileName)
     (error "Unable to find a corresponding file"))
)

(defun casey-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (casey-find-corresponding-file)
  (other-window -1)
)

(provide 'file-defuns)
