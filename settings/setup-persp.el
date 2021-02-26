;; -*- lexical-binding: t -*-
;;; `setup-persp.el' --- Summary: Persp/desktop configuration
;;; Commentary:
;;; Code:

(use-package desktop
  :config
  ;; Desktop sessions
  ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
  (unless (daemonp) (desktop-save-mode t))
  :custom
  (desktop-save 'if-exists)
  ;;(desktop-restore-reuses-frames 'keep)
  )

(use-package persp-mode
  :straight (persp-mode :type git :flavor melpa :host github :repo "Bad-ptr/persp-mode.el")
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers ivy-sort-functions-alist)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook (;; Has been known to interfere with dashbord
         ;; (after-init . persp-mode)
         (window-setup . toggle-frame-maximized))
  :init
  (setq persp-keymap-prefix (kbd "C-x p")
        persp-nil-name "default"
        persp-set-last-persp-for-new-frames nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-auto-resume-time 1.0
        persp-common-buffer-filter-functions
        (list
         #'(lambda (b)
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
      
      (define-ibuffer-filter persp-all
          "Toggle current view to buffers of all current perspectives."
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

  ;; Simplified variant. Add only current perspective group.
  (defun jawa/persp-mode-ibuffer-filter-groups-current ()

    (with-eval-after-load "ibuffer"
      (require 'ibuf-ext)

      (define-ibuffer-filter persp-current
          "Toggle current view to buffers of current perspective."
        (:description "persp-mode"
         :reader (persp-prompt nil nil (safe-persp-name (get-frame-persp)) t))
        (find buf (safe-persp-buffers (persp-get-by-name qualifier))))

      (defun persp-add-ibuffer-group ()
        (let ((perspslist (list (safe-persp-name (get-frame-persp))
                                (cons 'persp (safe-persp-name (get-frame-persp))))))
          (setq ibuffer-saved-filter-groups
                (delete* "persp-mode" ibuffer-saved-filter-groups
                         :test 'string= :key 'car))
          (push
           (cons "persp-mode" perspslist)
           ibuffer-saved-filter-groups)))

      (add-hook 'ibuffer-mode-hook
                #'(lambda ()
                    (persp-add-ibuffer-group)
                    (ibuffer-switch-to-saved-filter-groups "persp-mode")))))

  ;; Kill the buffer if it removed from every (or non weak) perspective
  (setq persp-autokill-buffer-on-remove 'kill-weak)

  ;; Don't save persp configs in `recentf'
  (push persp-save-dir recentf-exclude)

  ;; Ivy Integration - https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
  (with-eval-after-load 'ivy
    (add-to-list
     'ivy-ignore-buffers
     #'(lambda (b)
         (when persp-mode
           (let ((persp (get-current-persp)))
             (if persp
                 (not (persp-contain-buffer-p b persp))
               nil)))))
    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))

  ;; To quickly launch emacs to edit a single file without loading perspectives
  ;; Then in a bash terminal: emacs -persp-q <file>
  ;; Or, save in an editor.sh 
  ;; #!/bin/bash
  ;; emacs -persp-q $@;
  ;; Then:
  ;; export EDITOR="editor.sh"
  (add-to-list
   'command-switch-alist
   (cons "persp-q"
         #'(lambda (p)
             (setq persp-auto-resume-time -1
                   persp-auto-save-opt 0))))

  ;; Python inferior buffer save load
  (add-to-list 'persp-save-buffer-functions
               #'(lambda (b)
                   (when (eq 'inferior-python-mode (buffer-local-value 'major-mode b))
                     `(def-inferior-python-buffer ,(buffer-name b)
                        ,(let ((process (get-buffer-process b)))
                           (if process
                               (progn
                                 (python-shell-send-string "import os" process)
                                 (python-shell-send-string-no-output "os.getcwd()" process))
                             (concat "'" (buffer-local-value 'default-directory b) "'")))))))

  (add-to-list 'persp-load-buffer-functions
               #'(lambda (savelist)
                   (when (eq (car savelist) 'def-inferior-python-buffer)
                     (destructuring-bind (bname dir) (cdr savelist)
                       (run-python nil nil nil)
                       (with-current-buffer (python-shell-get-buffer)
                         (rename-buffer bname)
                         (cd dir)
                         (python-shell-send-string "import os")
                         (python-shell-send-string (format "os.chdir(%s)" dir))
                         (current-buffer))))))

  ;; Persp projectile auto persp
  (defvar persp-mode-projectile-bridge-before-switch-selected-window-buffer nil)

  ;; (setq persp-add-buffer-on-find-file 'if-not-autopersp)
  ;; https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-projectile-auto-persp-el
  (persp-def-auto-persp
   "projectile"
   :parameters '((dont-save-to-file . t)
                 (persp-mode-projectile-bridge . t))
   :hooks '(projectile-before-switch-project-hook
            projectile-after-switch-project-hook
            projectile-find-file-hook
            find-file-hook)
   :dyn-env '((after-switch-to-buffer-adv-suspend t))
   :switch 'frame
   :predicate
   #'(lambda (buffer &optional state)
       (if (eq 'projectile-before-switch-project-hook
               (alist-get 'hook state))
           state
         (and
          projectile-mode
          (buffer-live-p buffer)
          (buffer-file-name buffer)
          ;; (not git-commit-mode)
          (projectile-project-p)
          (or state t))))
   :get-name
   #'(lambda (state)
       (if (eq 'projectile-before-switch-project-hook
               (alist-get 'hook state))
           state
         (push (cons 'persp-name
                     (concat "p) "
                             (with-current-buffer (alist-get 'buffer state)
                               (projectile-project-name))))
               state)
         state))
   :on-match
   #'(lambda (state)
       (let ((hook (alist-get 'hook state))
             (persp (alist-get 'persp state))
             (buffer (alist-get 'buffer state)))
         (case hook
           (projectile-before-switch-project-hook
            (let ((win (if (minibuffer-window-active-p (selected-window))
                           (minibuffer-selected-window)
                         (selected-window))))
              (when (window-live-p win)
                (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer
                      (window-buffer win)))))

           (projectile-after-switch-project-hook
            (when (buffer-live-p
                   persp-mode-projectile-bridge-before-switch-selected-window-buffer)
              (let ((win (selected-window)))
                (unless (eq (window-buffer win)
                            persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                  (set-window-buffer
                   win persp-mode-projectile-bridge-before-switch-selected-window-buffer)))))

           (find-file-hook
            (setcdr (assq :switch state) nil)))
         (if (case hook
               (projectile-before-switch-project-hook nil)
               (t t))
             (persp--auto-persp-default-on-match state)
           (setcdr (assq :after-match state) nil)))
       state)
   :after-match
   #'(lambda (state)
       (when (eq 'find-file-hook (alist-get 'hook state))
         (run-at-time 0.5 nil
                      #'(lambda (buf persp)
                          (when (and (eq persp (get-current-persp))
                                     (not (eq buf (window-buffer (selected-window)))))
                            ;; (switch-to-buffer buf)
                            (persp-add-buffer buf persp t nil)))
                      (alist-get 'buffer state)
                      (get-current-persp)))
       (persp--auto-persp-default-after-match state)))

    ;; (add-hook 'persp-after-load-state-functions
    ;;           #'(lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)

  :bind
  (("C-c h P" . hydra-persp/body)
   ;; Global overrides
   ("C-x b" . persp-switch-to-buffer)
   ("C-x k" . persp-kill-buffer)))


;; Persp mode projectile integration
(use-package persp-mode-projectile-bridge
  :straight
  (persp-mode-projectile-bridge
   :type git :flavor melpa
   :host github :repo "Bad-ptr/persp-mode-projectile-bridge.el")
  :after (:all (persp-mode projectile-mode))
  :functions (persp-get-by-name
              persp-add-new
              persp-add-buffer
              set-persp-parameter
              my-persp-mode-projectile-bridge-add-new-persp)
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives
             persp-mode-projectile-bridge-add-new-persp
             projectile-project-buffers)
  :hook ((persp-mode . projectile-mode)
         (persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode .
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
