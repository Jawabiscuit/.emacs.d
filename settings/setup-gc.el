(use-package gcmh
  ;; ;TODO: 2019-12-02 - Package will not clone from my Github
  ;; :straight (gcmh :host github :repo "Jawabiscuit/gcmh"
  ;;                 :branch "master")
  :diminish gcmh-mode
  :config
  (gcmh-mode 1)
  :custom
  (gcmh-verbose nil)
  (gcmh-idle-delay 15))

(provide 'setup-gc)
