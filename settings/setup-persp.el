;;; setup-persp.el --- Configuration for persp-mode -*- lexical-binding: t -*-

;; Author: Jonas Avrin
;; URL: https://www.github.com/jawabiscuit
;; Package-Requires: (`recentf', `projectile')

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
;;

;;; Code:

(use-package persp-mode
  :after recentf
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers ivy-sort-functions-alist)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode)
         (window-setup . toggle-frame-maximized))
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time 1.0
              persp-common-buffer-filter-functions
              (list #'(lambda (b)
                        "Ignore temporary buffers."
                        (let ((bname (file-name-nondirectory (buffer-name b))))
                          (or (string-prefix-p " " bname)
                              (and (string-prefix-p "*" bname)
                                   (not (string-equal "*scratch*" bname)))
                              (string-suffix-p ".elc" bname)
                              (string-suffix-p ".gz" bname)
                              (string-suffix-p ".zip" bname)
                              (string-prefix-p "magit" bname)
                              (string-prefix-p "Pfuture-Callback" bname)
                              (string-match-p ".elfeed" bname)
                              (eq (buffer-local-value 'major-mode b) 'erc-mode)
                              (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
                              (eq (buffer-local-value 'major-mode b) 'nov-mode)
                              (eq (buffer-local-value 'major-mode b) 'vterm-mode))))))
  :config
  (defun jawa/persp-mode-ibuffer-filter-groups-all ()
    "Shows groups for all perspectives. But can't show same buffer in multiple groups."
    (with-eval-after-load "ibuffer"
      
      (require 'ibuf-ext)
      
      (define-ibuffer-filter persp
          "Toggle current view to buffers of current perspective."
        (:description "persp-mode"
                      :reader (persp-prompt nil nil (safe-persp-name (get-frame-persp)) t))
        (find buf (safe-persp-buffers (persp-get-by-name qualifier))))
      
      (defun persp-add-ibuffer-group ()
        (let ((perspslist (mapcar #'(lambda (pn)
                                      (list pn (cons 'persp pn)))
                                  (nconc
                                   (delete* persp-nil-name
                                            (persp-names-current-frame-fast-ordered)
                                            :test 'string=)
                                   (list persp-nil-name)))))
          (setq ibuffer-saved-filter-groups
                (delete* "persp-mode" ibuffer-saved-filter-groups
                         :test 'string= :key 'car))
          (push
           (cons "persp-mode" perspslist)
           ibuffer-saved-filter-groups)))
      
      (defun persp-ibuffer-visit-buffer ()
        (let ((buf (ibuffer-current-buffer t))
              (persp-name (get-text-property
                           (line-beginning-position) 'ibuffer-filter-group)))
          (persp-switch persp-name)
          (switch-to-buffer buf)))

      ;; TODO
      ;; (define-key ibuffer-mode-map (kbd "RET") 'persp-ibuffer-visit-buffer)
      
      (add-hook 'ibuffer-mode-hook
                #'(lambda ()
                    (persp-add-ibuffer-group)
                    (ibuffer-switch-to-saved-filter-groups "persp-mode")))))

  ;; Kill the buffer if it removed from every(or non weak) perspective
  (setq persp-autokill-buffer-on-remove 'kill-weak)

  ;; Don't save persp configs in `recentf'
  (push persp-save-dir recentf-exclude)

  ;; Ivy Integration - https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 #'(lambda (b)
                     (when persp-mode
                       (let ((persp (get-current-persp)))
                         (if persp
                             (not (persp-contain-buffer-p b persp))
                           nil))))))

  ;; To quickly launch emacs to edit a single file without loading perspectives
  ;; Then in a bash terminal: emacs -persp-q <file>
  ;; Or, save in an editor.sh 
  ;; #!/bin/bash
  ;; emacs -persp-q $@;
  ;; Then:
  ;; export EDITOR="editor.sh"
  (add-to-list 'command-switch-alist
               (cons "persp-q"
                     #'(lambda (p)
                         (setq persp-auto-resume-time -1
                               persp-auto-save-opt 0))))
  :bind
  (("C-c h P" . hydra-persp/body)))

;; Projectile integration
(use-package persp-mode-projectile-bridge
  :after projectile
  :functions (persp-get-by-name
              persp-add-new
              persp-add-buffer
              set-persp-parameter
              my-persp-mode-projectile-bridge-add-new-persp)
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives
             persp-mode-projectile-bridge-add-new-persp
             projectile-project-buffers)
  :hook ((persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          .
          (lambda ()
            (if persp-mode-projectile-bridge-mode
                (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
              (persp-mode-projectile-bridge-kill-perspectives)))))
  :init (setq persp-mode-projectile-bridge-persp-name-prefix "[p]")
  :config
  ;; HACK: Allow saving to files
  (defun my-persp-mode-projectile-bridge-add-new-persp (name)
    (let ((persp (persp-get-by-name name *persp-hash* :nil)))
      (if (eq :nil persp)
          (prog1
              (setq persp (persp-add-new name))
            (when persp
              (set-persp-parameter 'persp-mode-projectile-bridge t persp)
              (persp-add-buffer (projectile-project-buffers)
                                persp nil nil)))
        persp)))
  (advice-add #'persp-mode-projectile-bridge-add-new-persp
              :override #'my-persp-mode-projectile-bridge-add-new-persp))

(provide 'setup-persp)
;;; setup-persp.el ends here
