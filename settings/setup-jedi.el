(use-package python-environment)

;; NOTE: depends on hydra-posframe
;; The brains of Python autocomplete
(use-package emacs-jedi
  :straight (emacs-jedi :host github :repo "tkf/emacs-jedi")
  :after (hydra-posframe python-environment)
  :init
  ;; Uncomment next line if you like the menu right away
  (setq ac-show-menu-immediately-on-auto-complete t)
  ;; Can also express in terms of ac-delay var, e.g.:
  ;;   (setq ac-auto-show-menu (* ac-delay 2))
  ;; Enable Jedi setup on mode start
  (general-add-hook 'python-mode-hook 'jedi:setup)
  :config
  ;; Don't let tooltip show up automatically
  (setq jedi:get-in-function-call-delay 10000000)
  ;; Start completion at method dot
  (setq jedi:complete-on-dot t)
  :mode "\\.py\\'"
  :mode-hydra
  (python-mode
   (:tite "Jedi Commands")
   ("Goto"
    (("." jedi:goto-definition "Symbol definition")
     ("," jedi:goto-definition-pop-marker "Symbol definition, pops marker"))
    "Docs"
    (("?" jedi:show-doc "Show docstring")
     ("/" jedi:get-in-function-call "Manually show call signature")))))

(provide 'setup-jedi)
