(use-package smartparens
  ;; Unclutter the modeline
  :diminish smartparens
  :init
  (setq sp-autoescape-string-quote nil)
  :config
  (require 'smartparens-config)
  (--each '(css-mode-hook
            restclient-mode-hook
            js-mode-hook
            java-mode-hook
            ruby-mode-hook
            markdown-mode-hook
            emacs-lisp-mode-hook
            org-mode-hook
            python-mode-hook
            c++-mode-hook)
    (add-hook it 'turn-on-smartparens-mode))

  ;; Define diminished modes
  (eval-after-load "smartparens" '(diminish 'smartparens-mode))
  
  ;; Load diminished modes
  (turn-on-smartparens-mode)
  
  ;; elisp comment highlighting
  ;; Redefine this global pair (`LaTex') to a new value locally
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (sp-local-pair 'org-mode "=" "=" :wrap "C-+")
  
  ;; New pair
  (sp-pair "<" ">")
  
  ;; Local pairs can be removed by calling `sp-local-pair' with optional keyword argument `:actions' with value `:rem'
  ;; (sp-local-pair 'LaTeX-mode "`" nil :actions :rem)
  
  ;; Create a wrapping
  ;; Usage
  ;;
  ;; |foobar
  ;; hit C-(
  ;; becomes (|foobar)
  (sp-pair "(" ")" :wrap "C-("))


(provide 'setup-smartparens)
