;;; setup-dashboard.el --- Dashboard configuration -*- lexical-binding: t -*-

;; Author: Jonas Avrin
;; URL: https://www.github.com/jawabiscuit
;; Package-Requires: (`persp-mode', `recentf')

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

;;; Commentary:
;;
;; Inspired and borrowed seagle0128's Centaur dashboard
;;

;;; Code:

(defcustom jawa-logo (expand-file-name "images/logo.png" user-emacs-directory)
  "Set logo. nil means official logo."
  :group 'jawa
  :type 'string)

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 3)))
  "Emacs is 25.3 or above.")

(defvar jawa-dashboard t)
(unless emacs/>=25.3p (setq jawa-dashboard nil))

(when jawa-dashboard
  (use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :defines persp-special-last-buffer
    :functions (all-the-icons-faicon
                all-the-icons-material
                open-custom-file
                persp-get-buffer-or-null
                persp-switch-to-buffer
                winner-undo
                widget-forward)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :bind (("<f2>" . open-dashboard)
           :map dashboard-mode-map
           ("H" . browse-homepage)
           ("R" . restore-session)
           ("L" . persp-load-state-from-file)
           ("c" . open-custom-file)
           ("q" . quit-dashboard)
           ("h" . major-mode-hydra)
           ("?" . major-mode-hydra))
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
    :init
    (dashboard-setup-startup-hook)
    (defun with-material (icon str &optional height v-adjust)
      "Displays an icon from all-the-icon."
      (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
    :mode-hydra
    (dashboard-mode
       (:title (with-material "dashboard" "Dashboard" 1.1 -0.225)
        :quit-key "q")
       ("Navigator"
        (("b" browse-homepage "Browse Github" :exit t)
         ("s" restore-session "Recover session" :exit t)
         ("l" persp-load-state-from-file "List sessions" :exit t)
         ("c" open-custom-file "Settings" :exit t))
        "Section"
        ((">" dashboard-next-section "Next section")
         ("<" dashboard-previous-section "Previous section")
         ("r" dashboard-goto-recent-files "Recent files")
         ("m" dashboard-goto-bookmarks "Bookmarks")
         ("p" dashboard-goto-projects "Projects"))
        "Item"
        (("RET" widget-button-press "Open" :exit t)
         ("<tab>" widget-forward "Next")
         ("C-i" widget-forward "Next")
         ("<backtab>" widget-backward "Previous")
         ("C-n" next-line "Next line")
         ("C-p" previous-line "Previous line"))
        "Misc"
        (("<f2>" open-dashboard "Open dash" :exit t)
         ("g" dashboard-refresh-buffer "Refresh" :exit t)
         ("ESC" quit-dashboard "Quit dash" :exit t))))
    :config
    (setq dashboard-banner-logo-title "Program or be programmed..."
          dashboard-startup-banner (or jawa-logo 'official)
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((agenda . 5)
                            ;; (registers . 5)
                            (recents  . 10)
                            (bookmarks . 5)
                            (projects . 5))

          dashboard-week-agenda-p t
          
          dashboard-set-init-info t
          dashboard-set-file-icons t
          dashboard-set-heading-icons t
          dashboard-heading-icons '((recents   . "file-text")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")
                                    (projects  . "file-directory")
                                    (registers . "database"))

          dashboard-set-footer t
          dashboard-footer (format "Powered by biscuits, %s\n\nAdapted from: Centaur dashboard by Vincent Zhang" (format-time-string "%Y"))
          dashboard-footer-icon (cond ((display-graphic-p)
                                       (all-the-icons-faicon "heart"
                                                             :height 1
                                                             :v-adjust -0.01
                                                             :face 'font-lock-keyword-face))
                                      ((char-displayable-p ?♥) "♥ ")
                                      (t (propertize ">" 'face 'font-lock-doc-face)))

          dashboard-set-navigator t
          dashboard-navigator-buttons
          `(((,(when (display-graphic-p)
                 (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
              "Homepage" "Browse Homepage (H)"
              (lambda (&rest _) (browse-url "https://github.com")))
             (,(when (display-graphic-p)
                 (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
              "Restore" "Restore previous session (R)"
              (lambda (&rest _) (restore-session)))
             (,(when (display-graphic-p)
                 (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
              "Settings" "Open custom file (c)"
              (lambda (&rest _) (find-file custom-file)))
             (,(if (display-graphic-p)
                   (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
                 "?")
              "" "Help (?/h)"
              (lambda (&rest _) (major-mode-hydra))
              font-lock-string-face))))

    (defun my-banner-path (&rest _)
      "Return the full path to banner."
      (expand-file-name "banner.txt" user-emacs-directory))

    (advice-add #'dashboard-get-banner-path :override #'my-banner-path)

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun browse-homepage ()
      "Browse the Github."
      (interactive)
      (browse-url "https://github.com"))

    (defun open-custom-file()
      "Open custom.el if exists, otherwise create it."
      (interactive)
      (let ((custom-example
             (expand-file-name "custom-example.el" user-emacs-directory)))
        (unless (file-exists-p custom-file)
          (if (file-exists-p custom-example)
              (copy-file custom-example custom-file)
            (error "Unable to find \"%s\"" custom-example)))
        (find-file custom-file)))

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1))
             ;; exclude `treemacs' window
             (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
                 2
               1))
          (setq dashboard-recover-layout-p t))

      (delete-other-windows)

      ;; Refresh dashboard buffer
      (if (get-buffer dashboard-buffer-name)
          (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (goto-char (point-min))
      (dashboard-goto-recent-files))

    (defun restore-session ()
      "Restore last session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
          (error
           (message "Error: Unable to restore last session -- %s" err)))
        (quit-window t)
        (when (persp-get-buffer-or-null persp-special-last-buffer)
          (persp-switch-to-buffer persp-special-last-buffer))
        (message "Done")))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (funcall (local-key-binding "r")))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (funcall (local-key-binding "p")))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (funcall (local-key-binding "m")))))

(provide 'setup-dashboard)
;;; setup-dashboard.el ends here
