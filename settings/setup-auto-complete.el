;;; `setup-auto-complete.el' --- Summary: Setup auto completion
;;; Commentary:
;;; Code:


(use-package fuzzy
  :straight (fuzzy :host github :repo "auto-complete/fuzzy-el"))

(use-package auto-complete
  :diminish ac-mode
  :commands (ac-config-default)
  :bind
  ("C-c f" . ac-fuzzy-complete)
  :init
  (defun iqbal-auto-complete-at-point ()
    (when (and (not (minibufferp))
               auto-complete-mode
               (looking-back "\\(\\sw\\|\\s_\\)")
               (not (looking-at "\\sw\\|\\s_")))
      (auto-complete)))

  (defun set-auto-complete-as-completion-at-point-function ()
    (setq completion-at-point-functions
          (cons 'iqbal-auto-complete-at-point
                (remove 'iqbal-auto-complete-at-point completion-at-point-functions))))
  :hook ((auto-complete-mode . set-auto-complete-as-completion-at-point-function))
  :config
  (dolist (ac-mode '(text-mode
                     ;; org-mode
                     ))
    (add-to-list 'ac-modes ac-mode))

  (dolist (ac-mode-hook '(text-mode-hook org-mode-hook prog-mode-hook))
    (add-hook ac-mode-hook
              (lambda ()
                (setq ac-fuzzy-enable t)
                (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
                (add-to-list 'ac-sources 'ac-source-filename))))

  ;; Display completion correctly
  (setq popup-use-optimized-column-computation nil)

  ;; Use auto-complete with flyspell
  (with-eval-after-load 'auto-complete
    (define-key ac-menu-map [backtab] 'ac-previous)
    (ac-flyspell-workaround))

  ;; Maximum width of auto-complete menu
  (setq ac-max-width 0.5)

  ;; To get pop-ups with docs even if a word is uniquely completed
  ;; from Steve Purcell's config
  (setq-default ac-dwim nil)

  ;; Do not use pos-tip
  (setq ac-quick-help-prefer-pos-tip t)

  ;; Enable auto-complete globally
  ;; (global-auto-complete-mode)
)

(use-package company
  :after (:any python-mode org-mode)
  :straight (company :type git :flavor melpa :host github :repo "company-mode/company-mode")
  :diminish company-mode
  :bind (:map company-mode-map ("<s-tab>" . company-complete))
  :hook ((emacs-lisp-mode    . company-mode)
         (js2-mode           . company-mode)
         (web-mode           . company-mode)
         (css-mode           . company-mode)
         (c++-mode           . company-mode)
         (cider-repl-mode    . company-mode)
         (cider-mode         . company-mode)
         (sh-mode            . company-mode)
         (typescript-mode    . company-mode)
         (inferior-ess-mode  . company-mode)
         (ledger-mode        . company-mode)
         (python-mode        . company-mode)
         )
  :custom
  ;; Decrease delay before autocompletion popup shows
  (company-idle-delay                .4)
  (company-tooltip-idle-delay        0)
  ;; Bigger tooltip display
  (company-tooltip-limit             20)
  (company-minimum-prefix-length     1)
  (company-show-numbers              t)
  (company-dabbrev-downcase          nil)
  ;; Align annotations to the right tooltip border
  (company-tooltip-align-annotations 't)
  ;; Wrap around popup menu on reaching end
  (company-selection-wrap-around     t)
  ;; Allow non-matching input when company-mode is active
  (company-require-match             nil)
  ;; TODO company-emoji
  ;; Instruct company-emoji to not insert unicode
  ;; (company-emoji-insert-unicode      nil)
  ;; Do not enable company in some modes 
  (company-global-modes              '(not erlang-mode org-mode))
  ;; Start autocompletion only after typing
  ;; (company-begin-commands            '(self-insert-command))
  (company-transformers              '(company-sort-by-occurrence))
  :config
  (defun ma/reorder-argument-company-fill-propertize (orig-fun &rest args)
    "This advice to show number of candidate in company popup in left side"
    (if (string= " " (car (last args)))
        (apply orig-fun args)
      (apply orig-fun (append (butlast args 2) (reverse (last args 2))))))
  (advice-add #'company-fill-propertize :around #'ma/reorder-argument-company-fill-propertize)
  (add-to-list 'company-backends 'company-ispell)

  ;; Complete if only preview is being displayed
  ;; https://github.com/iqbalansari/dotEmacs
  (defun iqbal-complete-if-just-one-candidate ()
    (interactive)
    ;; This might fail sometimes since, it checks whether
    ;; inline preview CAN be displayed rather than whether it
    ;; IS displayed
    (call-interactively (if (company--show-inline-p)
                            'company-complete-selection
                          'company-abort)))

  (use-package company-web
    :defer t)

  (when (executable-find "auctex")
    (use-package company-auctex
      :defer t
      :init
      (company-auctex-init)
      (eval-after-load "company-auctex"
        ;; override this function, bad alignament in company
        '(defun company-auctex-symbol-annotation (candidate)
           nil))))

  (use-package company-anaconda
    :hook (python-mode-hook . anaconda-mode))

  (use-package company-quickhelp
    :hook (company-mode . company-quickhelp-mode))

  (use-package company-statistics
    :hook (company-mode . company-statistics-mode))

  ;; (global-company-mode t)
)

(provide 'setup-auto-complete)
;;; setup-auto-complete.el ends here
