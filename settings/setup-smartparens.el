;; Unclutter the modeline
(require 'diminish)

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode
          ruby-mode
          markdown-mode
          groovy-mode
          scala-mode)
  (add-hook it 'turn-on-smartparens-mode))

;; Define diminished modes
(eval-after-load "smartparens" '(diminish 'smartparens-mode))

;; Load diminished modes
(turn-on-smartparens-mode)

(provide 'setup-smartparens)
