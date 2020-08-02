;;; `org-defuns' --- Summary: Global org functions
;;; Commentary:
;;; Code:

;; Tramp defuns
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

;; Tangle defuns
(defun org-babel-tangle-jump ()
  "Jump to tangle file for the source block at point."
  (interactive)
  (let (file org-babel-pre-tangle-hook org-babel-post-tangle-hook)
    (cl-letf (((symbol-function 'write-region) (lambda (start end filename &rest _ignore)
                         (setq file filename)))
          ((symbol-function 'delete-file) #'ignore))
      (org-babel-tangle '(4)))
    (when file
      (setq file (expand-file-name file))
      (if (file-readable-p file)
      (find-file file)
    (error "Cannot open tangle file %S" file)))))

;; Structure Templates
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))

;; Toggle display of special markdown formatting characters in org buffers
(defun jawa/toggle-org-emphasis-markers (&optional arg)
  (interactive)
  "Toggle emphasis markers"
  (setq org-hide-emphasis-markers
        (if (null arg)
            (not org-hide-emphasis-markers)
          arg)))

;; Org mode links
;; `https://github.com/abo-abo/hydra/wiki/Org-mode-links'
(defun jk/unlinkify ()
  "Replace an org-link with the description, or if this is absent, the path."
  (interactive)
  (let ((eop (org-element-context)))
    (when (eq 'link (car eop))
      (message "%s" eop)
      (let* ((start (org-element-property :begin eop))
             (end (org-element-property :end eop))
             (contents-begin (org-element-property :contents-begin eop))
             (contents-end (org-element-property :contents-end eop))
             (path (org-element-property :path eop))
             (desc (and contents-begin
                        contents-end
                        (buffer-substring contents-begin contents-end))))
        (setf (buffer-substring start end)
              (concat (or desc path)
                      (make-string (org-element-property :post-blank eop) ?\s)))))))
