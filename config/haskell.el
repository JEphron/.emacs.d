(use-package lsp-haskell)

(reformatter-define haskell-format
  :program "ormolu")

(use-package haskell-mode
  :bind
  ("s-F" . haskell-format-buffer)
  ("C-M-r" . (lambda () (interactive) (async-shell-command "stack run") ))
  :hook
  (haskell-mode . haskell-format-on-save-mode)
  (haskell-mode . lsp))
