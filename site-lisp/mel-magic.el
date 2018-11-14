(add-hook 'outline-minor-mode-hook 
           (lambda () 
             (require 'outline-magic)
))
(add-hook 'mel-mode-hook 'my-mel-outline-hook)

(defun mel-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "    ")
      (current-column))))

(defun my-mel-outline-hook ()
  ;; (setq outline-regexp "[ \t]*# \\|[ \t]+\\(class\\|def\\|if\\|elif\\|else\\|while\\|for\\|try\\|except\\|with\\) ")
  (setq outline-regexp "\\(global proc\\|proc\\|[ \t]+if\\|[ \t]+while\\|[ \t]+for\\|[ \t]+switch\\|else\\|case\\) ")
  (setq outline-level 'mel-outline-level)

  (outline-minor-mode t)
  ;; (hide-body)
  (show-paren-mode 1)
  (define-key outline-minor-mode-map [S-tab] 'indent-for-tab-command)
  (define-key outline-minor-mode-map [M-down] 'outline-move-subtree-down)
  (define-key outline-minor-mode-map [M-up] 'outline-move-subtree-up)
)

(provide 'mel-magic)
