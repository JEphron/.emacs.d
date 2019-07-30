;;; config/default/config.el -*- lexical-binding: t; -*-

(use-package! d-mode
  :config
  ())

(use-package! flycheck-dmd-dub
  :when (featurep! :tools flycheck)
  :config
  (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables)
  (add-hook 'd-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   "dub"))))
