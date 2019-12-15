(defun open-agenda ()
  "Opens the org-agenda full screen"
  (interactive)
  (let ((agenda "*Org Agenda*"))
    (if (equal (get-buffer agenda) nil)
        (org-agenda-list)
      (unless (equal (buffer-name (current-buffer)) agenda)
        (switch-to-buffer agenda))
      (org-agenda-redo t)
      (beginning-of-buffer))))

(defun org-buffer-todo ()
  (interactive)
  "Creates a todo-list for the current buffer.
Equivalent to the sequence: org-agenda,
< (restrict to current buffer), t (todo-list)."
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-todo-list)))

(defun org-buffer-agenda ()
  (interactive)
  "Creates an agenda for the current buffer.
Equivalent to the sequence: org-agenda,
< (restrict to current buffer), a (agenda-list)."
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-agenda-list)))

(defun org-buffer-day-agenda ()
  (interactive)
  "Creates an agenda for the current buffer.
Equivalent to the sequence: org-agenda,
< (restrict to current buffer), a (agenda-list),
d (org-agenda-day-view)."
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-agenda-list)
    (org-agenda-day-view)))
