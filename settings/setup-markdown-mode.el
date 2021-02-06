;; -*- lexical-binding: t -*-
;;; `setup-markdown-mode.el' --- Summary: Configure Markdown major mode
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :config
  (setq-default markdown-hide-markup t)
  (setq markdown-command "pandoc")
  (setq markdown-imenu-generic-expression
      '(("title"  "^\\(.*\\)[\n]=+$" 1)
        ("h2-"    "^\\(.*\\)[\n]-+$" 1)
        ("h1"     "^# \\(.*\\)$" 1)
        ("h2"     "^## \\(.*\\)$" 1)
        ("h3"     "^### \\(.*\\)$" 1)
        ("h4"     "^#### \\(.*\\)$" 1)
        ("h5"     "^##### \\(.*\\)$" 1)
        ("h6"     "^###### \\(.*\\)$" 1)
        ("fn"     "^\\[\\^\\(.*\\)\\]" 1)))
  :mode (("README\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . pandoc-mode)
         (markdown-mode . (lambda ()
            (setq imenu-generic-expression
                  markdown-imenu-generic-expression)))))

(provide 'setup-markdown-mode)
;;; setup-markdown-mode.el ends here
