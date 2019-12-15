(use-package hydra)
;; NOTE: depends on hydra and posframe
(use-package hydra-posframe
  :straight (hydra-posframe :host github :repo "Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-mode))
(use-package major-mode-hydra
  :bind
  ("M-SPC" . major-mode-hydra))

;; Define some major mode hydras
(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(major-mode-hydra-define 'Info-mode
  (:title "Info navigation")
  ("Navigation"
   (("n" Info-next "next")
    ("p" Info-previous "previous")
    ("[" Info-backward-node "backward")
    ("]" Info-forward-node "forward")
    ("h" Info-up "up")
    ("m" Info-menu "menu")
    ("T" Info-toc "toc"))
   "History"
   (("l" Info-history-back "back")
    ("r" Info-history-forward "forward")
    ("L" Info-history "display"))
   "In this file"
   (("<" Info-top-node "beginning")
    (">" Info-final-node "end"))
   "Cross reference"
   (("f" Info-follow-reference "Follow"))))

(major-mode-hydra-define 'help-mode
  (:title "Help navigation")
  ("History"
   (("l" help-go-back "back")
    ("r" help-go-forward "forward"))
   "Button"
   (("f" help-follow-symbol "Follow"))))

(major-mode-hydra-define 'helpful-mode
  (:title "Helpful navigation")
  ("Buttons"
   (("n" forward-button "Forward")
    ("p" backward-button "Backward"))))

(provide 'setup-hydra)
