;; set to point to a particular virtualenv
(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")

;; variables to help find the project root
(defvar jedi-config:vcs-root-sentinel ".git")
(defvar jedi-config:python-module-sentinel "__init__.py")

;; function to find project root given a buffer
(defun get-project-root (buf repo-type init-file)
  (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))

;; define the jedi variable to find the root
(defvar jedi-config:find-root-function 'get-project-root)

;; call this on initialization
(defun current-buffer-project-root ()
  (funcall jedi-config:find-root-function
    (current-buffer)
    jedi-config:vcs-root-sentinel
    jedi-config:python-module-sentinel))

;; helper macro
;; read how to set server args [C-h v] jedi:server-args <RET>
;; store in local variable jedi:server-args
;; aim is for auto-completion to work with code in our project buffers
(defun jedi-config:setup-server-args ()
  (defmacro add-args (arg-list arg-name arg-value)
    '(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
  (let ((project-root (current-buffer-project-root)))
    ;; variable for this buffer only
    (make-local-variable 'jedi:server-args)
    ;; set the variables
    (when project-root
      (add-args jedi:server-args "--sys-path" project-root))
    (when jedi-config:with-virtualenv
      (add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv))))

;; find python
;; on some OSX systems this might need some tuning
;; (executable-find "python")
;; "c:/Users/Jonas/Anaconda3/envs/python27/python.exe"

;; putting everything together
(add-hook 'python-mode-hook
          'jedi-config:setup-server-args)
;; Symbol's value as variable is void: jedi-config:use-system-python
;; (when jedi-config:use-system-python
(add-hook 'python-mode-hook
          'jedi-config:set-python-executable)

;; Tweaks

;; If you really like the menu (so vividly named)
;; (setq ac-show-menu-immediately-on-auto-complete t)

;; set to an absurdly large delay if bound to a key
(setq jedi:get-in-function-call-delay 10000000)

;; complete when you type a dot
(setq jedi:complete-on-dot t)

(provide 'setup-jedi)
