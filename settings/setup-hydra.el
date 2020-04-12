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
  ;; Floating windows are making it impossible to use.
  :disabled t
  :straight (hydra-posframe :host github :repo "Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-mode))

(use-package major-mode-hydra
  :bind ("C-c M-SPC" . major-mode-hydra)
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
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-material (icon str &optional height v-adjust)
      "Displays an icon from all-the-icon."
      (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

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

(defvar hydra-dired-title (with-faicon "compass" "Dired Navigation" 1.5 -0.05))

(major-mode-hydra-define 'dired-mode
  (:title hydra-dired-title
   :quit-key "q")
  ("Navigation"
   (("i" dired-subtree-insert "Insert" :exit nil)
    ("/" dired-subtree-apply-filter "Filter" :exit nil)
    ("k" dired-subtree-remove "Kill" :exit nil)
    ("n" dired-subtree-next-sibling "Next sibling" :exit nil)
    ("p" dired-subtree-previous-sibling "Prev sibling" :exit nil)
    ("j" dired-subtree-up "Up" :exit nil)
    ("k" dired-subtree-down "Down" :exit nil)
    ("b" dired-subtree-beginning "Beginning" :exit nil)
    ("e" dired-subtree-end "End" :exit nil)
    ("m" dired-subtree-mark-subtree "Mark" :exit nil)
    ("u" dired-subtree-unmark-subtree "Unmark" :exit nil)
    ("C-f" dired-subtree-only-this-file "Remove this file" :exit t)
    ("C-d" dired-subtree-only-this-directory "Remove this directory" :exit t))
   "View"
   (("RET" dired-subtree-toggle "Toggle" :exit nil)
    ("c" dired-subtree-cycle "Cycle" :exit nil)
    (">" dired-subtree-narrow "Narrow" :exit t)
    ("r" dired-subtree-revert "Revert" :exit t))
   "Open"
   (("x" dired-open-xdg "xdg-open"))))

(defvar hydra-toggles-title (with-faicon "toggle-on" "Toggles" 1.5 -0.05))

(pretty-hydra-define hydra-toggles
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-toggles-title
   :foreign-keys warn
   ;; :color blue
   :quit-key "q"
  )
  ("Focus Mode"
   (("v" toggle-frame-fullscreen "Fullscreen view" :toggle t))
   "Info/check/linting Modes"
   (("e" eldoc-mode "Echo Lisp objs" :toggle t)
    ("a" apheleia-mode "Code format" :toggle t)
    ("A" apheleia-global-mode "Format global" :toggle t)
    ("fc" flycheck-mode "Code linter" :toggle t)
    ("fs" flyspell-mode "Spell check" :toggle t)
    ("fp" flyspell-prog-mode "Spell check prog" :toggle t)
    ("fv" flycheck-verify-setup "Verify setup"))
    ;; ("ld" lsp-ui-doc-mode :toggle t)
    ;; ("lp" lsp-ui-peek-mode :toggle t)
    ;; ("ls" lsp-ui-sideline-mode :toggle t))
   "Edit/assistance"
   (("C-p" persp-mode-projectile-bridge-mode "Projectile bridge mode" :toggle t)
    ("C-j" ja-keys-minor-mode "My keys minor mode" :toggle t)
    ("C-a" global-auto-complete-mode "AC global" :toggle t)
    ("C-l" electric-layout-mode "Elec layout" :toggle t)
    ("C-i" electric-indent-local-mode "Elec indent" :toggle t)
    ("C-q" electric-quote-local-mode "Elec quote" :toggle t)
    ("C-g" aggressive-indent-mode "Aggro indent" :toggle t)
    ("C-w" toggle-word-wrap "Word wrap" :toggle t)
    ("C-t" toggle-truncate-lines "Trunc lines" :toggle t)
    ("C-s" yas-minor-mode "Yas" :toggle t)
    ("C-c" whitespace-cleanup-mode "Whtspc cleanup" :toggle t)
    ("C-f" auto-fill-mode "Autofill" :toggle t) ; TODO: Toggle face does not change
    ("C-y" lispy-mode "Lispy" :toggle t))
   "Visual"
   (("e" jawa/toggle-org-emphasis-markers "Org emphasis" :toggle t)
    ("o" origami-mode "Origami" :toggle t)
    ("n" linum-mode "Linum" :toggle t)
    ("w" whitespace-mode "Whtspc" :toggle t)
    ("p" page-break-lines-mode "Page break lines" :toggle t)
    ("g" global-git-gutter-mode "Git gutter" :toggle t)
    ("i" fci-mode "Fill column ind" :toggle t)
    ("C-i" highlight-indent-guides-mode "Hilite indent" :toggle t)
    ("C-r" ivy-filthy-rich-mode "Ivy filty rich" :toggle t)
    ("ESC" nil "Quit"))))

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
   "Web"
   (("i" org-web-tools-insert-link-for-url :exit t)
    ("p" org-web-tools-insert-web-page-as-entry :exit t)
    ("C-l" org-insert-link-global "Insert link")
    ("s" org-store-link "Store link")
   )
   "Navigation"
   (("C->" org-toggle-narrow-to-subtree "Narrow/Widen" :toggle t)
    ("C-j" outline-next-visible-heading "Next visible headline")
    ("C-k" outline-previous-visible-heading "Prev visible headline")
    ("C-n" org-forward-heading-same-level "Fwd to heading, same lvl")
    ("C-p" org-backward-heading-same-level "Bwd to heading, same lvl")
    ("u" outline-up-heading "Up to parent heading")
    ("g" org-goto "Different loc in current file")
    ("ESC" nil "Quit"))))

(defvar hydra-windows-title (with-faicon "windows" "Windows" 1.5 -0.05))

(pretty-hydra-define hydra-windows
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-windows-title
   :foreign-keys warn
   :quit-key "q"
  )
  ("Window"
   (("9" balance-windows "Balance" :exit t)
    ("i" enlarge-window "Scale up")
    ("k" shrink-window "Scale down")
    ("j" shrink-window-horizontally "Scale down X")
    ("l" enlarge-window-horizontally "Scale up X"))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("=" text-scale-increase "in")
    ("0" (text-scale-increase 0) "reset"))
   "Switch"
   (("a" (lambda ()
      (interactive)
      (ace-window 1)
      (add-hook 'ace-window-end-once-hook
                'hydra-window/body))
     "Ace switch")
    ("s" (lambda ()
      (interactive)
      (ace-swap-window)
      (add-hook 'ace-window-end-once-hook
                'hydra-window/body))
     "Ace swap")
    ("m" switch-window-then-maximize "Maximize")
    ("b" switch-window-then-split-below "Split below")
    ("r" switch-window-then-split-right "Split right")
    ("d" switch-window-then-delete "Delete" :exit t)
    ("D" switch-window-then-dired "Dired" :exit t)
    ("f" switch-window-then-find-file "Find file" :exit t)
    ("F" switch-window-then-find-file-read-only "Find file r/o" :exit t)
    ("c" switch-window-then-compose-mail "Compose mail" :exit t)
    ("B" switch-window-then-display-buffer "Display buffer")
    ("k" switch-window-then-kill-buffer "Kill buffer")
    ("ESC" nil "Quit"))))

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
    ("s" counsel-ag "Ag search" :exit t)
    ("ESC" nil "Quit"))))

(defvar hydra-origami-title (with-octicon "unfold" "Origami" 1.5 -0.05))

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
     ("o" origami-show-only-node "Show Only" :exit t))
   "Navigation"
    (("k" origami-next-fold "Fwd >>")
     ("j" origami-previous-fold "Bwd <<")
     ("ESC" nil "Quit"))))

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
    ("g" avy-flycheck-goto-error "Goto error")
    ("ESC" nil "Quit"))))

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
    ("p" flyspell-check-previous-highlighted-word "Auto correct previous highlighted" :exit t)
    ("b" flyspell-buffer "Check entire buffer")
    ("c" flyspell-auto-correct-word "Current word")
    ("ESC" nil "Quit"))))

(defvar hydra-yankpad-title (with-faicon "pencil-square" "Yankpad" 1.5 -0.05))

;; NOTE about `yankpad' and aya (`auto-yasnippet')
;; You can also add snippets to the current `yankpad-category' by
;; using M-x `yankpad-capture', or with M-x `yankpad-aya-persist'
;; if you're an `auto-yasnippet' user.
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
    ("c" yankpad-capture-snippet "Capture snippet" :exit t)
    ("ESC" nil "Quit"))))

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
    ("C" smeargle-clear "Clear" :exit t)
    ("ESC" nil "Quit"))))

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
   (("C-s" org-timer-start "Start timer")
    ("C-q" org-timer-stop "Stop timer")
    ("C-t" org-timer-set-timer "Set timer (at timer)")
    ("c" org-clock-cancel "Cancel")
    ("e" org-clock-modify-effort-estimate "Effort" :exit t)
    ("i" org-clock-in "Clock-in" :exit t)
    ("g" org-clock-goto "Go-to")
    ("o" org-clock-out "Clock-out" :exit t)
    ("r" org-clock-report "Report")
    ("s" org-schedule "Schedule" :exit t)
    ("d" org-deadline "Deadline" :exit t)
    ("ESC" nil "Quit"))))

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
    ("ESC" nil "Quit"))))

(defvar hydra-mc-title (with-faicon "i-cursor" "Multiple Cursors" 1.5 -0.05))

(pretty-hydra-define hydra-mc
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-mc-title
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Mark" (
    ("p" mc/mark-previous-like-this "Mark prev")
    ("P" mc/unmark-previous-like-this "Unmark prev")
    ("n" mc/mark-next-like-this "Mark next")
    ("N" mc/unmark-next-like-this "Unmark next")
    ("C-v" mc/cycle-forward "Cycle Fwd")
    ("C-S-v" mc/cycle-backward "Cycle Bwd")
    ("r" mc/remove-current-cursor "Remove cursor")
    ("j" mc/skip-to-previous-like-this "Skip to prev")
    ("k" mc/skip-to-next-like-this "Skip to next")
    ("a" mc/mark-all-dwim "Mark all dwim")
    ("A" mc/mark-all-like-this "Mark all")
    ("x" mc/mark-all-in-region-regexp "Region regex" :exit t)
    ("c" mc/move-to-column "Column")
    ("C-S-b" mc/mark-all-below "Mark below")
    ("C-S-a" mc/mark-all-above "Mark above")
    ("C-t" mc/mark-sgml-tag-pair "Mark tag pair")
    ("C-h" mc-hide-unmatched-lines-mode "Hide unmatched"))
   "Edit" (
    ("l" mc/edit-lines "Edit lines" :exit t)
    ("b" mc/edit-beginnings-of-lines "Edit beginnings" :exit t)
    ("e" mc/edit-ends-of-lines "Edit ends" :exit t)
    ("fu" mc/unfreeze-fake-cursors "Unfreeze fake")
    ("ff" mc/freeze-fake-cursors "Freeze fake"))
   "Remove" (
    ("fr" mc/remove-fake-cursor "Remove fake")
    ("C-e" mc/remove-cursors-at-eol "Remove eol")
    ("C-b" mc/remove-cursors-at-bol "Remove bol")
    ("C-l" mc/remove-cursors-on-blank-lines "Remove blank lines")
    ("C-d" mc/remove-duplicated-cursors "Remove duplicated")
    ("hd" hungry-delete "Delete white space")
    ("hb" hungry-backspace "Backspace white space"))
   "Transform" (
    ("v" mc/vertical-align "Vertical align" :exit t)
    ("V" mc/vertical-align-with-space "Vertical align space" :exit t)
    ("#" mc/insert-numbers "Insert numbers")
    ("C-s r" mc/sort-regions "Sort regions")
    ("C-s v" mc/reverse-regions "Reverse regions")
    ("ESC" nil "Quit"))))

(defvar hydra-delims-title (with-faicon "quote-left" "Delims" 1.5 -0.05))

(pretty-hydra-define hydra-delims
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-delims-title
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Toggle"
   (("RET" smartparens-mode :toggle t)
    ("s" smartparens-strict-mode :toggle t)
    ("r" rainbow-delimiters-mode :toggle t))
   "Wrap"
   (("(" sp-wrap-round "Wrap ()")
    ("{" sp-wrap-curly "Wrap {}")
    ("[" sp-wrap-square "Wrap []")
    ("u" sp-unwrap-sexp "Unwrap"))
   "Point"
   (("." sp-forward-symbol "Fwd symbol")
    ("," sp-backward-symbol "Bwd symbol")
    (">" sp-forward-sexp "Fwd expression")
    ("<" sp-backward-sexp "Bwd expression"))
   "Edit"
   (("k" sp-kill-sexp "Kill")
    ("K" sp-backward-kill-sexp "Bwd kill expression")
    ("'" sp-forward-slurp-sexp "Fwd slurp")
    (";" sp-backward-slurp-sexp "Bwd slurp")
    ("C-'" sp-forward-barf-sexp "Fwd barf")
    ("C-;" sp-backward-barf-sexp "Bwd barf")
    ("W" sp-forward-whitespace "Fwd kill whitespace")
    ("w" sp-backward-whitespace "Bwd kill whitespace")
    ("ESC" nil "Quit"))))

(defvar hydra-persp-title (with-material "dashboard" "Persp" 1.5 -0.05))

(pretty-hydra-define hydra-persp
  (:pre (setq which-key-inhibit t)
   :post (setq which-key-inhibit nil)
   :title hydra-persp-title
   :hint nil
   :foreign-keys warn
   :quit-key "q"
  )
  ("Persp"
   (("RET" persp-mode :toggle t)
    ("b" persp-mode-projectile-bridge-mode :toggle t)
    ("n" persp-next "Next")
    ("p" persp-prev "Prev")
    ("s" persp-switch "Create/switch" :exit t)
    ("S" persp-window-switch "Create/switch in a window" :exit t)
    ("r" persp-rename "Rename" :exit t)
    ("c" persp-copy "Copy" :exit t)
    ;; Killing "default" (nil buf) will kill all buffers
    ("C" persp-kill "Kill persp" :exit t)
    ("a" persp-add-buffer "Add buffer")
    ("b" persp-switch-to-buffer "Switch to buffer")
    ("t" persp-temporarily-display-buffer "Display temp buffer" :exit t)
    ("i" persp-import-buffers "Import buffers" :exit t)
    ("I" persp-import-win-conf "Import win config" :exit t)
    ;; With prefix argument reverses the effect of the persp-autokill-buffer-on-remove
    ("k" persp-remove-buffer "Remove buffer")
    ("K" persp-kill-buffer "Kill buffer")
    ("w" persp-save-state-to-file "Save all to file" :exit t)
    ("W" persp-save-to-file-by-names "Save some to file" :exit t)
    ("l" persp-load-state-from-file "Load all from file" :exit t)
    ("L" persp-load-from-file-by-names "Load some from file" :exit t)
    ("ESC" nil "Quit"))))

(provide 'setup-hydra)
;;; setup-hydra.el ends here
