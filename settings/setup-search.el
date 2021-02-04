;; -*- lexical-binding: t -*-
;;; `setup-search.el' --- Summary: Searching utilities
;;; Commentary:
;;; Code:

;; Silver searcher
(if (executable-find "ag")
    (use-package ag
      :init
      (with-eval-after-load 'projectile
        (bind-key "C-c p s s" 'ag-project projectile-mode-map))
      :config
      (setq ag-highlight-search t)
      (setq ag-reuse-buffers t)
      )
  (message "ag was not found on the path. ag package was not loaded."))

;; Allows you to edit a grep buffer
;; and apply those changes to the file buffer
(use-package wgrep)

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; Ripgrep
(if (executable-find "rg")
    (use-package rg
      :hook
      ((after-init . rg-enable-default-bindings)
       (rg-mode . wgrep-ag-setup))
      ;; :init
      ;; (add-hook 'after-init-hook #'rg-enable-default-bindings)
      ;; (if (fboundp 'wgrep-ag-setup)
      ;;     (add-hook 'rg-mode-hook #'wgrep-ag-setup))
      ;; (with-eval-after-load 'projectile
      ;;   (bind-key "C-c p s r" 'rg-project projectile-mode-map))
      :config
      (setq rg-custom-type-aliases nil)
      (setq rg-group-result t)
      (setq rg-show-columns t)
      ;; (with-eval-after-load 'counsel
      ;;   (bind-key "c" 'counsel-rg rg-global-map))
      :bind (:map rg-global-map
                  ("c" . counsel-rg)
                  :map projectile-mode-map
                  ("C-c p s r" . rg-project))
      )
  (message "rg was not found on the path. rg package was not loaded."))

;; Platinum grep
(if (executable-find "pt")
    (use-package pt
      :init
      (with-eval-after-load 'projectile
        (bind-key "C-c p s p" 'projectile-pt projectile-mode-map))
      )
  (message "pt was not found on the path. pt package was not loaded."))

;; CTRLF
(use-package ctrlf
  :straight (:host github
             :repo "raxod502/ctrlf")
  :defer t
  :init (ctrlf-mode +1)
  :bind (:map minibuffer-local-map
              ("C-r" . ctrlf-backward-literal)))

(provide 'setup-search)
;;; setup-search.el ends here
