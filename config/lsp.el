(use-package lsp-mode
  :after evil-collection
  :bind (
	 ("C-j" . lsp-ui-doc-glance)
	 :map evil-normal-state-map ;; todo: merge evil maps (general?)
	 ("C-b" . lsp-find-definition)
	 :map evil-insert-state-map
	 ("C-b" . lsp-find-definition)))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil))
