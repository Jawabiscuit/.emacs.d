
(use-package hydra
  :bind (("C-c h o" . hydra-org/body)
         ("C-c h t" . hydra-toggles/body)
         ("C-c h w" . hydra-windows/body)
))

(use-package hydra-posframe
  :after (hydra posframe)
  :straight (hydra-posframe :host github :repo "Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-mode))

(use-package major-mode-hydra
  :after (hydra all-the-icons)
  :bind ("M-SPC" . major-mode-hydra)
  :preface
  (require 'all-the-icons)
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

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

(defvar hydra-toggles-title (with-faicon "toggle-on" "Toggles" 1.5 -0.05))

(pretty-hydra-define hydra-toggles
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-toggles-title
   :foreign-keys warn
   :color blue
   :quit-key "q")
  ("Focus"
   (("v" view-mode :toggle t))
   "Info/check/linting"
   (("fd" eldoc-mode :toggle t)
    ("fc" flycheck-mode :toggle t)
    ("fv" flycheck-verify-setup)
    ("fs" flyspell-mode :toggle t)
    ("fp" flyspell-prog-mode :toggle t)
    ("a" apheleia-mode :toggle t)
    ("A" apheleia-global-mode :toggle t)
    ("ld" lsp-ui-doc-mode :toggle t)
    ("lp" lsp-ui-peek-mode :toggle t)
    ("ls" lsp-ui-sideline-mode :toggle t))
   "Edit/assistance"
   (("s" smartparens-mode :toggle t)
    ("p" show-paren-mode :toggle t)
    ("S" smartparens-strict-mode :toggle t)
    ("y" lispy-mode :toggle t)
    ("el" electric-layout-mode :toggle t)
    ("ei" electric-indent-local-mode :toggle t)
    ("eq" electric-quote-local-mode :toggle t)
    ("ea" aggressive-indent-mode :toggle t)
    ("o" origami-mode :toggle t)
    ("W" whitespace-cleanup-mode))
   "Visual"
   (("w" whitespace-mode :toggle t)
    ("r" rainbow-delimiters-mode :toggle t)
    ("p" page-break-lines-mode :toggle t)
    ("n" linum-mode :toggle t)
    ("hi" highlight-indent-guides-mode :toggle t)
    ("hc" fci-mode :toggle t))
   ;; "LSP"
   ;; (("lh" lsp-describe-session)
   ;;  ("lR" lsp-restart-workspace)
   ;;  ("lS" lsp-shutdown-workspace))
))

(defvar hydra-org-title (with-fileicon "org" "Org" 1.5 -0.05))

(pretty-hydra-define hydra-org
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-org-title
   :foreign-keys warn
   ;; :color teal
   :quit-key "q")
  ("Action"
   (("a" org-agenda "Agenda")
    ("c" org-capture "Capture")
    ("l" org-capture-goto-last-stored "Last capture stored")
    ("d" org-decrypt-entry "Decrypt")
    ("k" org-cut-subtree "Cut subtree")
    ("r" org-refile "Refile")
    ("o" org-open-at-point-global "Open link")
   )
   "Clock"
   (("hs" org-timer-start "Start timer")
    ("hq" org-timer-stop "Stop timer")
    ("ht" org-timer-set-timer "Set timer (at timer)")
    ("hn" (org-clock-in '(4)) "Clock in")
    ("ho" org-clock-out "Clock out")
    ("hg" org-clock-goto "Clock goto")
   )
   "Web"
   (("i" org-web-tools-insert-link-for-url :exit t)
    ("p" org-web-tools-insert-web-page-as-entry :exit t)
    ("hl" org-insert-link-global "Insert link")
    ("s" org-store-link "Store link")
   )
   "Navigation"
   (("hj" outline-next-visible-heading "Next visible headline")
    ("hk" outline-previous-visible-heading "Prev visible headline")
    ("h." org-forward-heading-same-level "Fwd to heading, same lvl")
    ("h," org-backward-heading-same-level "Bwd to heading, same lvl")
    ("u" outline-up-heading "Up to parent heading")
    ("g" org-goto "Different loc in current file")
   )))

(defvar hydra-windows-title (with-faicon "windows" "Windows" 1.5 -0.05))

(pretty-hydra-define hydra-windows
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-windows-title
   :foreign-keys warn
   :quit-key "q")
  ("Window"
   (("b" balance-windows "balance")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("l" enlarge-window-horizontally "widen")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

(provide 'setup-hydra)

