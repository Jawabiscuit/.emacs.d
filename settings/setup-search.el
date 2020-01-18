;;; setup-search --- Packages for grepping projects -*- lexical-binding: t -*-

;; Author: Jonas Avrin
;; URL: https://www.github.com/jawabiscuit
;; Package-Requires: (`projectile')

;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:
;; 
;; Search utils: `ag', `rg', `pt'

;;; Code:

;; Silver searcher
(use-package ag
  :init
  (with-eval-after-load 'projectile
    (bind-key "C-c p s s" 'ag-project projectile-mode-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

;; Allows you to edit a grep buffer
;; and apply those changes to the file buffer
(use-package wgrep)

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; Ripgrep
(use-package rg
  :after (projectile ivy)
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
	 ("C-c p s r" . rg-project)))

;; Platinum grep
(use-package pt
  :disabled t				; Executable not installed yet
  :init
  (with-eval-after-load 'projectile
    (bind-key "C-c p s p" 'projectile-pt projectile-mode-map)))

(provide 'setup-search)
;;; .el ends here
