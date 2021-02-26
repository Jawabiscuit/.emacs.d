;; -*- lexical-binding: t -*-
;;; `setup-etom.el' --- Summary: Emacs to Maya
;;; Commentary:
;;; Code:

(defun jawa/etom-hook ()
  "Emacs to Maya setup."
  (require 'etom)
  ;; Macro: Execute src block in Maya
  (fset 'etom-src-block
        [?\C-c ?\C-v ?\C-v ?\C-x ?h ?\C-c C-M-return ?\C-x ?\C-s])
  ;; Macro: Tangle then execute in Maya
  (fset 'etom-tangle-src-block
        [?\C-c ?\C-v ?\C-t ?\M-x ?t ?a ?n ?g ?l ?e ?  ?j ?u ?m return ?\C-x ?h ?\C-c C-M-return ?\C-c C-x ?k])
  ;; Customize
  (setq etom-default-host "localhost")
  (setq etom-default-port 2222)
  ;; Keybindings
  (jawa/bind-user "<C-M-return>" #'etom-send-region)
  (jawa/bind-user "C-S-c" #'etom-send-buffer)
  (jawa/bind-user "C-S-b" #'etom-show-buffer)
  (jawa/bind-user "<C-return>" #'etom-src-block)
  (jawa/bind-user "C-v <C-return>" #'etom-tangle-src-block)
  (jawa/bind-user "C-S-c" #'etom-send-buffer))

(general-add-hook
 '(org-mode-hook python-mode-hook)
 #'jawa/etom-hook
 t)

(provide 'setup-etom)
;;; setup-etom.el ends here
