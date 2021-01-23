;;; `setup-spell-check.el' --- Summary: Setup spell check tools
;;; Commentary:
;;; Code:

(when (executable-find "hunspell")
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US")
  (when sys/wslp (setq ispell-alternate-dictionary "/usr/share/hunspell/en_US.dic"
                       ispell-local-dictionary-alist
                       '(("english"
                          "[[:alpha:]]"
                          "[^[:alpha:]]"
                          "[']"
                          t
                          ("-d" "en_US" "-p" "/usr/share/hunspell")
                          nil
                          iso-8859-1))
                       ispell-dictionary "english"))
  (when sys/win32p (setq ispell-program-name "hunspell"
                         ispell-complete-word-dict
                         (concat (expand-file-name user-emacs-directory)
                                 "hunspell/share/hunspell/en_US.dic")))
  (when sys/linuxp (setq ispell-program-name "hunspell"
                         ispell-complete-word-dict
                         (concat (expand-file-name user-emacs-directory)
                                 "hunspell/share/hunspell/en_US.dic"))))

(use-package flyspell
  :when (executable-find "hunspell")
  :bind (("C-c h s" . hydra-flyspell/body))
  :config
  (general-unbind
    :keymaps 'flyspell-mode-map
    :package 'flyspell
    "C-," "C-." "C-M-i" "C-c $" "C-;"))


(provide 'setup-spell-check)
;;; setup-spell-check.el ends here
