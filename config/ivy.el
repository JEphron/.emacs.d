(defun je/find-in-path (start end) (interactive "r")
       (if (use-region-p)
	   (deactivate-mark
	    (counsel-rg (buffer-substring-no-properties start end)))
	 (counsel-rg)))

(use-package counsel :demand
  :after evil
  ;; fuzzy searching thing
  :bind (("M-A" . counsel-M-x)
	 ("M-F" . je/find-in-path)
	 :map evil-normal-state-map
	 ("/" . swiper)
	 :map evil-visual-state-map
	 ("/" . (lambda () (interactive)
		  (deactivate-mark)
 		  (swiper (buffer-substring-no-properties
			   (region-beginning) (region-end))))))
  :config
  (setq ivy-initial-inputs-alist nil
	ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (ivy-mode 1)
  (counsel-mode 1))

(use-package prescient
  ;; hopefully sort better
  :after ivy)

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1)
  (add-to-list 'ivy-sort-functions-alist '(counsel-recentf . nil)))

(use-package ivy-rich :ensure t
  :config
  (setq ivy-rich-path-style 'abbrev
	ivy-rich-display-transformers-list
	'(ivy-switch-buffer
	  (:columns
	   ((ivy-rich-candidate (:width 20))
	    (ivy-rich-switch-buffer-size (:width 7 :align right))
	    (ivy-rich-switch-buffer-indicators
	     (:width 2 :face error :align right))
	    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
	    (ivy-rich-switch-buffer-project (:width 8 :face success))
	    (ivy-rich-switch-buffer-path
	     (:width (lambda (x)
		       (ivy-rich-switch-buffer-shorten-path
			x (ivy-rich-minibuffer-width 0.3))))))
	   :predicate (lambda (cand) (get-buffer cand)))
	  counsel-M-x
	  (:columns
	   ((counsel-M-x-transformer (:width 40))
	    (ivy-rich-counsel-function-docstring
	     (:face font-lock-doc-face))))
	  counsel-describe-function
	  (:columns
	   ((counsel-describe-function-transformer (:width 40))
	    (ivy-rich-counsel-function-docstring
	     (:face font-lock-doc-face))))
	  counsel-describe-variable
	  (:columns
	   ((counsel-describe-variable-transformer (:width 40))
	    (ivy-rich-counsel-variable-docstring
	     (:face font-lock-doc-face))))
	  counsel-recentf
	  (:columns
	   ((ivy-rich-candidate (:width 0.8))
	    (ivy-rich-file-last-modified-time
	     (:face font-lock-comment-face))))))

  (ivy-rich-mode 1))
