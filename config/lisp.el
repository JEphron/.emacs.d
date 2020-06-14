;; (load (expand-file-name
;;        "~/.quicklisp/slime-helper.el"))

(use-package repl-toggle
  :config
  (setq rtog/mode-repl-alist '((lisp-mode . slime))))

(use-package slime
  :general
  (:keymaps 'slime-repl-mode-map
            :states '('normal 'insert)
            "C-p" 'slime-repl-previous-input
            "C-n" 'slime-repl-next-input
            "C-k" 'evil-delete-line
            "C-l" 'slime-repl-clear-buffer
            "C-e" 'end-of-line
            "C-a" 'beginning-of-line
            "C-b" 'backward-char
            "C-f" 'forward-char
            "C-d" 'evil-delete-line)
  ;; (:keymaps 'slime-mode-map
  ;;           "C-c C-s" 'je/toggle-slime-repl)
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup))

;; (evil-collection-define-key 'normal 'insert 'slime-repl-mode-map
;;   "C-p" 'slime-repl-previous-input
;;   "C-n" 'slime-repl-next-input)

(use-package lispy
  :config
  (add-hook 'lisp-mode
            (lambda () (lispy-mode 1))))

(use-package lispyville
  :bind
  ("C-{" . lispyville-insert-at-beginning-of-list)
  ("C-}" . lispyville-insert-at-end-of-list)
  :init
  (general-add-hook
   '(emacs-lisp-mode-hook lisp-mode-hook)
   #'lispyville-mode)
  :config
  (setq lispyville-barf-stay-with-closing 't)
  (lispyville-set-key-theme
   '(operators
     c-w
     additional
     commentary
     slurp/barf-cp)))
