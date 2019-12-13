(use-package ivy
  :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq enable-recursive-minibuffers t)
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
)

(use-package ivy-filthy-rich
  :straight (ivy-filthy-rich :host github :repo "akirak/ivy-filthy-rich"
                             :branch "fix-max-length")
  ;; :diminish 'ivy-filthy-rich-mode
  :after ivy
  :config
  (ivy-filthy-rich-mode t)
  ;; :custom
  ;; (ivy-filthy-rich-max-length 120)
)

(provide 'setup-ivy)
