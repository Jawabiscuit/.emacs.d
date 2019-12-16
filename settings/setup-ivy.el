(use-package ivy
  :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq enable-recursive-minibuffers t)
  (add-to-list 'ivy-sort-functions-alist
        '(read-file-name-internal . eh-ivy-sort-file-by-mtime))
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (ivy-mode 1)
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-d" . counsel-find-dir)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("H-u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox))
  :custom
  (enable-recursive-minibuffers t)
  ;; (ivy-height 10)
  (ivy-initial-inputs-alist nil "Don't prepend `^' to any of the ivy prompts")
  (projectile-completion-system 'ivy)
  (ivy-ignore-buffers (quote ("\\` "))))

(use-package ivy-filthy-rich
  :straight (ivy-filthy-rich :host github :repo "akirak/ivy-filthy-rich"
                             :branch "fix-max-length")
  ;; :diminish 'ivy-filthy-rich-mode
  :after ivy
  :config
  (ivy-filthy-rich-mode t)
  :custom
  (ivy-filthy-rich-max-length 120))

;; Various completion functions using Ivy, Swiper for search
(use-package counsel
  :config
  (counsel-mode 1))

;; https://github.com/abo-abo/swiper/wiki/Sort-files-by-mtime#a-simple-version
(defun eh-ivy-sort-file-by-mtime (x y)
  (let* ((x (concat ivy--directory x))
         (y (concat ivy--directory y))
         (x-mtime (nth 5 (file-attributes x)))
         (y-mtime (nth 5 (file-attributes y))))
    (if (file-directory-p x)
        (if (file-directory-p y)
            (time-less-p y-mtime x-mtime)
          t)
      (if (file-directory-p y)
          nil
        (time-less-p y-mtime x-mtime)))))

(defcustom akirak/ivy-posframe-width-alist nil "Alist of height.")

(use-package ivy-posframe
  ;; Use posframe to display candidates in ivy commands.
  ;;
  ;; 1. The default display function is ivy-posframe-display-at-frame-center.
  ;;    However, if there is an EXWM window in the frame, use
  ;;    ivy-posframe-display-at-window-center instead.
  ;; 2. For swiper commands, display the posframe at the window bottom.
  ;; 3. There may be specific situations where I prefer other display functions.
  ;;
  ;; If the current focus is on an EXWM window, ivy-posframe is never used.
  ;; Instead, the default display function is used. This is configured in setup-exwm.el.
  :config
  (general-add-hook 'ivy-height-alist akirak/ivy-posframe-height-alist)
  (ivy-posframe-mode 1)

  (defun akirak/frame-contains-exwm-window-p (&optional frame)
    "Detect exwm mode in use"
    (--any (eq 'exwm-mode (buffer-local-value 'major-mode (window-buffer it)))
           (window-list frame)))

  (defun akirak/posframe-poshandler-smart-center (info)
    "Handle smart centering of displayed info"
    (let ((posframe-width (if-let ((pixel-width (plist-get info :posframe-width)))
                              (/ pixel-width (frame-char-width))
                            90))
          (window-width (window-width))
          (frame-width (frame-width))
          (has-exwm-window (akirak/frame-contains-exwm-window-p)))
      (cond
       ((and has-exwm-window (< window-width posframe-width))
        (posframe-poshandler-window-bottom-left-corner info))
       (has-exwm-window
        (posframe-poshandler-window-center info))
       ((< frame-width posframe-width)
        (posframe-poshandler-frame-bottom-left-corner info))
       (t
        (posframe-poshandler-frame-center info)))))
  
  (defun akirak/ivy-posframe-window-bottom-left-size ()
    "Calculate width and height"
    (list
     :height ivy-posframe-height
     :width (window-body-width)
     ;; :min-height (or ivy-posframe-min-height (+ ivy-height 1))
     ;; :min-width (or ivy-posframe-min-width (round (* (frame-width) 0.62)))
     ))

  (defun akirak/ivy-posframe-default-size ()
    "The default functon used by `ivy-posframe-size-function'."
    (let ((caller (ivy-state-caller ivy-last)))
      (list
       :height (or (cdr (assoc caller akirak/ivy-posframe-height-alist))
                   ivy-posframe-height)
       :width (or (cdr (assoc caller akirak/ivy-posframe-width-alist))
                  ivy-posframe-width)
       :min-height (or ivy-posframe-min-height (+ ivy-height 1))
       :min-width (unless (member caller '(ivy-omni-org
                                           all-the-icons-ivy))
                    (or ivy-posframe-min-width
                        (round (* (frame-width) 0.62)))))))

  (defun akirak/ivy-posframe-display-smart-center (str)
    "Display centered info"
    (ivy-posframe--display str #'akirak/posframe-poshandler-smart-center))

  (defun akirak/ivy-decorator-width ()
    (let ((caller (ivy-state-caller ivy-last)))
      (cdr (assoc caller akirak/ivy-posframe-width-alist))))

  :config/el-patch
  (el-patch-defun ivy-posframe-display-at-window-bottom-left (str)
    (el-patch-wrap 1
      (let ((ivy-posframe-size-function #'akirak/ivy-posframe-window-bottom-left-size))
        (ivy-posframe--display str #'posframe-poshandler-window-bottom-left-corner))))

  :custom
  (ivy-decorator-width #'akirak/ivy-decorator-width)
  (ivy-posframe-height 12)
  (ivy-posframe-width 100)
  (akirak/ivy-posframe-width-alist
   `((counsel-ibuffer . 120)
     (ivy-omni-org . 80)
     (all-the-icons-ivy . 50)
     ,@(--map (cons it 130)
              '(counsel-describe-function
                counsel-describe-variable
                counsel-faces
                counsel-M-x))))
  (akirak/ivy-posframe-height-alist
   '((ivy-omni-org . 30)
     (all-the-icons-ivy . 30)))
  (ivy-posframe-size-function #'akirak/ivy-posframe-default-size)
  (org-starter-swiper-width-function (lambda () (- (window-body-width) 5)))
  (ivy-posframe-display-functions-alist
   `(,@(--map (cons it nil)
              '(swiper swiper-all swiper-multi org-starter-swiper-config-files))
     (counsel-minibuffer-history . nil)
     (counsel-yank-pop . ivy-posframe-display-at-point)
     (all-the-icons-ivy . ivy-posframe-display-at-point)
     (t . akirak/ivy-posframe-display-smart-center))))

(use-package counsel-projectile)

(provide 'setup-ivy)
