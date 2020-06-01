(use-package ace-jump-mode
  :after evil-collection
  :bind (
	 :map evil-normal-state-map
	 ("C-." . ace-jump-mode)
	 :map evil-insert-state-map
	 ("C-." . ace-jump-mode)))
