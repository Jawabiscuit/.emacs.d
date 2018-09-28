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
(setq scroll-step 3)

;; Clock
(display-time)

;; Startup windowing
;; (setq next-line-add-newlines nil)
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil)
;; (split-window-horizontally)


;; Syntax (disabled)
;(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
;(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
;(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
;(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
;(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
;(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
;(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
;(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
;(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")

(provide 'appearance)
