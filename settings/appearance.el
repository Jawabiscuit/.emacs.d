;; Don't beep. Don't visible-bell (fails on el capitan). Just blink the modeline on errors.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Highlight current line
(global-hl-line-mode 1)

;; Fonts
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
(set-face-attribute 'default t :font "DejaVu Sans Mono-12")

;; Smooth scroll
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Clock
(display-time)

;; Startup windowing (disabled)
;; (setq next-line-add-newlines nil)
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil)
;; (split-window-horizontally)

;; Bright-red TODOs
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
    (font-lock-add-keywords
     mode
     '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
      ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
     fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

(provide 'appearance)
