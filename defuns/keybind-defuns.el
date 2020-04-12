;; `keybind-defuns' --- Summary: For working with keybindings
;;; Commentary: https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
;;; Code:

(defun key-binding-at-point (key)
  "KEY Helper function."
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))

(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

(defun keymaps-at-point ()
  "List entire keymaps present at point."
  (interactive)
  (let ((map-list
         (list
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'keymap))
                  (overlays-at (point)))
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'local-map))
                  (overlays-at (point)))
          (get-text-property (point) 'keymap)
          (get-text-property (point) 'local-map))))
    (apply #'message
           (concat
            "Overlay keymap: %s\n"
            "Overlay local-map: %s\n"
            "Text-property keymap: %s\n"
            "Text-property local-map: %s")
           map-list)))

;; Eg. (my-lookup-key (kbd "C-c m"))
(defun my-lookup-key (key)
  "Search for KEY in all known keymaps."
  (mapatoms (lambda (ob) (when (and (boundp ob) (keymapp (symbol-value ob)))
                           (when (functionp (lookup-key (symbol-value ob) key))
                             (message "%S" ob))))
            obarray))

(provide 'keybind-defuns)
;;; keybind-defuns.el ends here


