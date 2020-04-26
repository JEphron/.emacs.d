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

;; cosmetics
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(use-package soothe-theme)
(load-theme 'soothe t)

(use-package eval-sexp-fu)

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package counsel :demand
  ;; fuzzy searching thing
  :bind ("M-A" . counsel-M-x)
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
  :after (ivy))

(use-package ivy-prescient
  :after (prescient)
  :config
  (ivy-prescient-mode 1))

(use-package projectile
  :bind
  ("M-p" . projectile-find-file)
  ("M-P" . projectile-switch-project)
  ("C-M-p" . projectile-find-file-in-known-projects)
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
  (evil-mode 1))

(use-package evil-collection
  :after (evil))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package evil-cleverparens
  :after (evil)
  :hook ((emacs-lisp-mode . evil-cleverparens-mode)
	 (racket-mode . evil-cleverparens-mode))
  )

;; language modes
(use-package racket-mode)

;; keys I want
;;    find file in project
;;    jump to project
;;    run action (M-S-a)
;;    go to definition (M-b)
;;

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)


;; todo:
;; check out some interesting configs:
;;   https://git.sr.ht/~tslil/dotfiles/tree/0de26bf98f8ce2360c5e0614909ab349c8393eb8/emacs/init.el
