;;; `setup-hydra.el' --- Setup hydra packages and pretty hydras  -*- lexical-binding: t -*-
;;
;; Author: Jonas Avrin
;; Maintainer: Jonas Avrin
;; Version: 0.0.1
;; Package-Requires: (`all-the-icons')
;; Homepage:
;; Keywords:
;;
;;
;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package hydra
  :bind (("C-c h t" . hydra-toggles/body)
         ("C-c h k" . hydra-clock/body)
         ("C-c h r" . hydra-straight/body)))

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
   ;; :color blue
   :quit-key "q"
  )
  ("Focus"
   (("v" view-mode :toggle t)
   )
   "Info/check/linting"
   (("fd" eldoc-mode :toggle t)
    ("fc" flycheck-mode :toggle t)
    ("fv" flycheck-verify-setup)
    ("fs" flyspell-mode :toggle t)
    ("fp" flyspell-prog-mode :toggle t)
    ("a" apheleia-mode :toggle t)
    ("A" apheleia-global-mode :toggle t)
    ;; ("ld" lsp-ui-doc-mode :toggle t)
    ;; ("lp" lsp-ui-peek-mode :toggle t)
    ;; ("ls" lsp-ui-sideline-mode :toggle t)
   )
   "Edit/assistance"
   (("el" electric-layout-mode :toggle t)
    ("ei" electric-indent-local-mode :toggle t)
    ("eq" electric-quote-local-mode :toggle t)
    ("ea" aggressive-indent-mode :toggle t)
    ("o" origami-mode :toggle t)
    ("W" whitespace-cleanup-mode :toggle t)
    ("ew" toggle-word-wrap :toggle t)
    ("et" toggle-truncate-lines :toggle t)
    ("ey" yas-minor-mode :toggle t)
    ("F" auto-fill-mode :toggle t) ; TODO: Toggle face does not change
    ("y" lispy-mode :toggle t)
   )
   "Visual"
   (("s" smartparens-mode :toggle t)
    ("S" smartparens-strict-mode :toggle t)
    ("hp" turn-on-show-smartparens-mode :toggle t) ; TODO: Toggle face does not change
    ("r" rainbow-delimiters-mode :toggle t)
    ("n" linum-mode :toggle t)
    ("w" whitespace-mode :toggle t)
    ("p" page-break-lines-mode :toggle t)
    ("g" global-git-gutter-mode :toggle t)
    ("hi" highlight-indent-guides-mode :toggle t)
    ("hr" fci-mode :toggle t)
    ("iv" ivy-filthy-rich-mode :toggle t)
   )
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
   :quit-key "q"
  )
  ("Action"
   (("a" org-agenda "Agenda" :exit t)
    ("c" org-capture "Capture" :exit t)
    ("l" org-capture-goto-last-stored "Last capture stored")
    ("d" org-decrypt-entry "Decrypt")
    ("k" org-cut-subtree "Cut subtree")
    ("r" org-refile "Refile")
    ("o" org-open-at-point-global "Open link" :exit t)
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
   :quit-key "q"
  )
  ("Window"
   (("b" balance-windows "Balance" :exit t)
    ("i" enlarge-window "Scale up")
    ("j" shrink-window-horizontally "Scale down X")
    ("k" shrink-window "Scale down")
    ("l" enlarge-window-horizontally "Scale up X"))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))
   "Switch"
   (("f" counsel-projectile-find-file "find file" :exit t)
    ("a" (lambda ()
      (interactive)
      (ace-window 1)
      (add-hook 'ace-window-end-once-hook
                'hydra-window/body))
     "switch" :exit t)
    ("s" (lambda ()
      (interactive)
      (ace-swap-window)
      (add-hook 'ace-window-end-once-hook
                'hydra-window/body))
     "swap" :exit t))))

(defvar hydra-projectile-title (with-faicon "rocket" "Projectile" 1.5 -0.05))

(pretty-hydra-define hydra-projectile
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-projectile-title
   :color green
   :foreign-keys warn
   :quit-key "q"
  )
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "Switch to buffer" :exit t)
    ("k" projectile-kill-buffers "Kill all" :exit t)
    ("S" projectile-save-project-buffers "Save all" :exit t))
   "Find"
   (("d" counsel-projectile-find-dir "Find directory" :exit t)
    ("D" projectile-dired "Dired root" :exit t)
    ("f" counsel-projectile-find-file "Find file" :exit t)
    ("p" counsel-projectile-switch-project "Switch project" :exit t))
   "Other"
   (("i" projectile-invalidate-cache "Reset cache" :exit t))
   "Search"
   (("r" projectile-replace "Search/replace")
    ("R" projectile-replace-regexp "Regexp replace")
    ("s" counsel-ag "Ag search"))))

(defvar hydra-origami-title (with-faicon "connectdevelop" "Origami" 1.5 -0.05))

(pretty-hydra-define hydra-origami
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-origami-title
   :color red
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("State"
    (("RET" origami-mode :toggle t)
     ("u" origami-undo "Undo")
     ("r" origami-redo "Redo")
     ("x" origami-reset "Reset"))
   "Fold"
    (("<" origami-open-node "Open")
     (">" origami-close-node "Close")
     ("f" origami-forward-toggle-node "Fwd toggle")
     ("a" origami-toggle-all-nodes "All toggle")
     ("s" origami-show-node "Show")
     ("o" origami-show-only-node "Show Only" :exit t) ; TODO: Duplicate, displays only one
     ("o" origami-show-only-node "Show Only" :exit t))
   "Navigation"
    (("k" origami-next-fold "Fwd >>")
     ("j" origami-previous-fold "Bwd <<"))))

(defvar hydra-flycheck-title (with-faicon "check-circle" "Flycheck" 1.5 -0.05))

(pretty-hydra-define hydra-flycheck
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-flycheck-title
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Misc"
   (("RET" global-flycheck-mode :toggle t)
    ("f" flycheck-error-list-set-filter "Filter")
    ("s" flycheck-select-checker "Select checker"))
   "Navigation"
   (("j" flycheck-next-error "Next error")
    ("k" flycheck-previous-error "Previous error")
    ("<" flycheck-first-error "First error")
    (">" flycheck-previous-error "Previous error")
    ("g" avy-flycheck-goto-error "Goto error") ; TODO: Duplicate, displays only one
    ("g" avy-flycheck-goto-error "Goto error"))))

(defvar hydra-flyspell-title (with-faicon "check-circle" "Flyspell" 1.5 -0.05))

(defun akirak/turn-on-flyspell-mode-in-text-mode ()
  "Turn flyspell on when hydra head is activated."
  (when (derived-mode-p 'text-mode)
    (flyspell-mode t)))

(pretty-hydra-define hydra-flyspell
  (:pre
   (setq which-key-inhibit t)
   :pre
   (akirak/turn-on-flyspell-mode-in-text-mode)
   :post (setq which-key-inhibit nil)
   :title hydra-flyspell-title
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Auto-correct"
   (("n" flyspell-goto-next-error "Next error")
    ("p" flyspell-check-previous-highlighted-word "Auto correct previous highlighted")
    ("b" flyspell-buffer "Check entire buffer")
    ("c" flyspell-auto-correct-word "Current word") ; TODO: Duplicate, displays only one
    ("c" flyspell-auto-correct-word "Current word"))))

(defvar hydra-yankpad-title (with-faicon "pencil-square" "Yankpad" 1.5 -0.05))

(pretty-hydra-define hydra-yankpad
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-yankpad-title
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Yankpad"
   (("C" yankpad-set-category "Set category")
    ("A" yankpad-append-category "Append category")
    ("i" yankpad-insert "Insert" :exit t)
    ("a" yankpad-aya-persist "Aya persist")
    ("c" yankpad-capture-snippet "Capture snippet" :exit t) ; TODO: Duplicate, displays only one
    ("c" yankpad-capture-snippet "Capture snippet" :exit t))))

(defvar hydra-magit-title (with-octicon "octoface" "Magit" 1.5 -0.05))

(pretty-hydra-define hydra-magit
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-magit-title
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Action"
   (("b" magit-blame "Blame")
    ("c" magit-clone "Clone" :exit t)
    ("i" magit-init "Init" :exit t)
    ("l" magit-log-buffer-file "Commit log (current file)" :exit t)
    ("L" magit-log-current "Commit log (project)" :exit t)
    ("m" magit-status "Status" :exit t)
    ("M" magit-status-fullscreen "Status FS" :exit t) ; TODO: Duplicate, displays only one
    ("M" magit-status-fullscreen "Status FS" :exit t))
  "Git Gutter"
   (("g" global-git-gutter-mode :toggle t)
    ("k" git-gutter:previous-hunk "Prev hunk")
    ("u" git-gutter:update-all-windows "Refresh visible bufs")
    ("j" git-gutter:next-hunk "Next hunk")
    ("S" git-gutter:stage-hunk "Stage hunk" :exit t)
    ("R" git-gutter:revert-hunk "Revert hunk" :exit t))
  "Time Machine"
   (("RET" git-timemachine-toggle :toggle t :exit t))
  "Smeargle"
   (("h" smeargle "Activate")
    ("c" smeargle-commits "Commits")
    ("C" smeargle-clear "Clear" :exit t))))


(defvar hydra-clock-title (with-octicon "clock" "Clock" 1.5 -0.05))

(pretty-hydra-define hydra-clock
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-clock-title
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Clock"
   (("c" org-clock-cancel "Cancel")
    ("e" org-clock-modify-effort-estimate "Effort" :exit t)
    ("i" org-clock-in "Clock-in" :exit t)
    ("g" org-clock-goto "Go-to")
    ("o" org-clock-out "Clock-out" :exit t)
    ("r" org-clock-report "Report")
    ("s" org-schedule "Schedule" :exit t)
    ("d" org-deadline "Deadline" :exit t))))

(defvar hydra-straight-title (with-octicon "package" "Straight" 1.5 -0.05))

(pretty-hydra-define hydra-straight
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-straight-title
   :title "Straight"
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Straight"
   (("c" straight-check-all "Check all")
    ("C" straight-check-package "Check Package")
    ("r" straight-rebuild-all "Rebuild all" :exit t)
    ("R" straight-rebuild-package "Rebuild packge" :exit t)
    ("f" straight-fetch-all "Fetch" :exit t)
    ("F" straight-fetch-package "Fetch package" :exit t)
    ("p" straight-pull-all "Pull all" :exit t)
    ("P" straight-pull-package "Pull package" :exit t)
    ("m" straight-merge-all "Merge all" :exit t)
    ("M" straight-merge-package "Merge package" :exit t)
    ("n" straight-normalize-all "Normalize all" :exit t)
    ("N" straight-normalize-package "Normalize package" :exit t)
    ("u" straight-push-all "Push all" :exit t)
    ("U" straight-push-package "Push package" :exit t)
    ("v" straight-freeze-versions "Freeze versions" :exit t)
    ("V" straight-thaw-versions "Thaw versions" :exit t)
    ("w" straight-watcher-start "Watcher start" :exit t)
    ("W" straight-watcher-stop "Watcher stop" :exit t)
    ("g" straight-get-recipe "Get recipe" :exit t)
    ("e" straight-prune-build "Prune build" :exit t)
    )))

(provide 'setup-hydra)
;;; setup-hydra.el ends here
