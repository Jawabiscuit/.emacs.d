;;; `agenda-defuns' --- Summary: Useful macros for working with agendas
;;; Commentary:
;;; Code:
(defun open-agenda ()
  "Opens the `org-agenda' full screen."
  (interactive)
  (let ((agenda "*Org Agenda*"))
    (if (equal (get-buffer agenda) nil)
        (org-agenda-list)
      (unless (equal (buffer-name (current-buffer)) agenda)
        (switch-to-buffer agenda))
      (org-agenda-redo t)
      (beginning-of-buffer))))

(defun org-buffer-todo ()
  "Create a todo-list for the current buffer.
Equivalent to the sequence: `org-agenda',
< (restrict to current buffer), t (todo-list)."
  (interactive)
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-todo-list)))

(defun org-buffer-agenda ()
  "Create an agenda for the current buffer.
Equivalent to the sequence: `org-agenda',
< (restrict to current buffer), a (agenda-list)."
  (interactive)
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-agenda-list)))

(defun org-buffer-day-agenda ()
  "Create an agenda for the current buffer.
Equivalent to the sequence: `org-agenda',
< (restrict to current buffer), a (agenda-list),
d (org-agenda-day-view)."
  (interactive)
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-agenda-list)
    (org-agenda-day-view)))

(provide 'agenda-defuns)
;;; agenda-defuns.el ends here
