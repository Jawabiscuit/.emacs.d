(defun endless/insert-key (key)
  "Ask for a key then insert its description.
   Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((is-org-mode (derived-mode-p 'org-mode))
         (tag (if is-org-mode
                  "@@html:<kbd>%s</kbd>@@"
                "<kbd>%s</kbd>")))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char (if is-org-mode -8 -6)))
   ))

(defun append-as-kill ()
  "Performs copy-region-as-kill as an append."
  (interactive)
  (append-next-kill) 
  (copy-region-as-kill (mark) (point)))

;; Duplicate line
;; https://stackoverflow.com/a/88828
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; Move line or region up/down
;; https://stackoverflow.com/a/2426347
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n"))

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1))

;; https://scripter.co/splitting-an-org-block-into-two/
(defun modi/org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.

The Org block can be *any*: src, example, verse, etc., even any
Org Special block.

This function is heavily adapted from `org-between-regexps-p'."
  (save-match-data
    (let ((pos (point))
          (case-fold-search t)
          (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
          (limit-up (save-excursion (outline-previous-heading)))
          (limit-down (save-excursion (outline-next-heading)))
          beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
                 (re-search-backward block-begin-re limit-up :noerror))
             (setq beg (match-beginning 0))
             ;; ... and BLOCK-END-RE after it...
             (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                         (match-string-no-properties 1)
                                         "\\( .*\\)*$")))
               (goto-char (match-end 0))
               (re-search-forward block-end-re limit-down :noerror))
             (> (setq end (match-end 0)) pos)
             ;; ... without another BLOCK-BEGIN-RE in-between.
             (goto-char (match-beginning 0))
             (not (re-search-backward block-begin-re (1+ beg) :noerror))
             ;; Return value.
             (cons beg end))))))

(defun modi/org-split-block ()
  "Sensibly split the current Org block at point."
  (interactive)
  (if (modi/org-in-any-block-p)
      (save-match-data
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (at-bol (bolp))
                block-start
                block-end)
            (save-excursion
              (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
              (setq block-start (match-string-no-properties 0))
              (setq block-end (replace-regexp-in-string
                               "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                               (match-string-no-properties 1))))
            ;; Go to the end of current line, if not at the BOL
            (unless at-bol
              (end-of-line 1))
            (insert (concat (if at-bol "" "\n")
                            block-end
                            "\n\n"
                            block-start
                            (if at-bol "\n" "")))
            ;; Go to the line before the inserted "#+begin_ .." line
            (beginning-of-line (if at-bol -1 0)))))
    (message "Point is not in an Org block")))

