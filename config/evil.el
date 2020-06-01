
(use-package evil :demand
  :bind
  (:map evil-window-map
        ("-" . evil-window-split)
        ("|" . evil-window-vsplit))
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t)))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired
                                    eshell
                                    eww
                                    git-timemachine
                                    ibuffer
                                    image
                                    image+))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-cleverparens
  :after evil
  :hook ((emacs-lisp-mode . evil-cleverparens-mode)
         (racket-mode . evil-cleverparens-mode)))
