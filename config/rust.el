(use-package rust-mode
  :bind
  ("C-M-r" . rust-run)
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode-hook . (lambda () (setq indent-tabs-mode nil))))
