;; Bright-red TODOs
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)

(mapc (lambda (mode)
    (font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
     fixme-modes
)

(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

;; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
    (append
       '(; ("\\.html\\'"     . crappy-jsp-mode)
	 ("\\.tag$"        . html-mode)
	 ("\\.vm$"         . html-mode)
	 ("\\.ejs$"        . html-mode)
	 ("\\.scss$"       . css-mode)
	 ("\\.md$"         . markdown-mode)
	 ("\\.markdown$"   . markdown-mode)
	 ("\\.org$"        . org-mode)
	 ("\\.cpp$"        . c++-mode)
         ("\\.hin$"        . c++-mode)
         ("\\.cin$"        . c++-mode)
         ("\\.inl$"        . c++-mode)
         ("\\.rdc$"        . c++-mode)
         ("\\.h$"          . c++-mode)
         ("\\.c$"          . c++-mode)
         ("\\.cc$"         . c++-mode)
         ("\\.c8$"         . c++-mode)
         ("\\.txt$"        . indented-text-mode)
         ("\\.emacs$"      . emacs-lisp-mode)
         ("\\.gen$"        . gen-mode)
         ("\\.ms$"         . fundamental-mode)
         ("\\.m$"          . objc-mode)
         ("\\.mm$"         . objc-mode)
	 ("\\.restclient$" . restclient-mode)
         )
    auto-mode-alist)
)

(provide 'mode-mappings)
