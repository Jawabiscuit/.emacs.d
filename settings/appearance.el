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

;; Unclutter the modeline
(require 'diminish)

;; Define diminished modes
(eval-after-load "smartparens" '(diminish 'smartparens-mode))

;; Load diminished modes
(turn-on-smartparens-mode)

(provide 'appearance)
