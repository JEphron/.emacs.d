(load-theme 'wheatgrass t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)

;; show recent files on startup
(setq initial-buffer-choice 'counsel-recentf)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; auto-close parens and quotes
(electric-pair-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; cosmetics
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
;; (use-package soothe-theme)

;; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; font size stuff
(defun je/change-font-scale (amnt)
  (set-face-attribute 'default nil :height
		    (+ (face-attribute 'default :height) amnt)))

(defun je/increase-font-scale () (interactive)
  (je/change-font-scale 10))

(defun je/decrease-font-scale () (interactive) 
  (je/change-font-scale -10))

(bind-key "<C-S-prior>" 'je/increase-font-scale)
(bind-key "<C-S-next>" 'je/decrease-font-scale)

;; packages 
(use-package eval-sexp-fu)
(use-package aggressive-indent)

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package counsel :demand
  ;; fuzzy searching thing
  :bind ("M-A" . counsel-M-x) ("M-F" . counsel-rg)
  :config
  (setq ivy-initial-inputs-alist nil
	ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (ivy-mode 1)
  (counsel-mode 1))

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

(use-package prescient
  ;; hopefully sort better
  :after ivy)

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1)
  (add-to-list 'ivy-sort-functions-alist '(counsel-recentf . nil)))

(use-package projectile
  :bind
  ("C-M-p" . projectile-switch-project)
  ("M-P" . projectile-find-file)
  ;; ("M-p" . projectile-find-file-in-known-projects)
  :config
  (setq projectile-completion-system 'ivy
	Projectile-sort-order 'recently-active
	projectile-indexing-method 'hybrid
	projectile-project-search-path '("~/dev/work" "~/dev/personal"))
  (projectile-mode 1))

(use-package evil :demand
  ;; vimlike
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t)))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired eshell eww git-timemachine ibuffer image image+))
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

(use-package ace-jump-mode
  :after evil-collection
  :bind (
	 :map evil-normal-state-map
	 ("C-." . ace-jump-mode)
	 :map evil-insert-state-map
	 ("C-." . ace-jump-mode)))

(use-package which-key
  ;; show possible key-chord completions
  :config
  (setq which-key-idle-delay 0.2)
  (which-key-mode 1))

;; dependencies
(use-package f)
(use-package s)
(use-package emacs)
(use-package dash)
(use-package reformatter)

(use-package popwin
  :config
  (popwin-mode 1))

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

(use-package flycheck)
(use-package company)

;; language modes
(use-package racket-mode)
(use-package rust-mode
  :bind
  ("C-M-r" . rust-run)
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode-hook . (lambda () (setq indent-tabs-mode nil))))

;; haskell
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

(use-package elm-mode
  :after (f s emacs dash reformatter))

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)


;; todo:
;; check out some interesting configs:
;;   https://git.sr.ht/~tslil/dotfiles/tree/0de26bf98f8ce2360c5e0614909ab349c8393eb8/emacs/init.el
