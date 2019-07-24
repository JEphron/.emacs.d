(require 'seq)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; disable annoying macos fullscreen animation
(setq ns-use-native-fullscreen nil)

;; put the backup files somewhere else
(setq backup-directory-alist '(("." . "~/.saves"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; straight to work
(setq inhibit-startup-screen t)

;; Titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; -- PACKAGES --

;; Evil master race
(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1)
  ;; make Evil use Emacs's word boundries
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-set-initial-state 'term-mode 'emacs))

;; comment things out
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

;; makes it so pressing * with a visual selection works as expected
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode 1))

;; S) cs([, etc
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Ivy
(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  ;; counsel-rg gets extra space
  (add-to-list 'ivy-height-alist '(counsel-rg . 30)))
  

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))


(use-package hydra
  :ensure t)

(use-package ivy-hydra
  :ensure t)

;; improved m-x experience
(use-package amx
  :ensure t
  :config
  (amx-initialize))

;; Show key hints
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-idle-delay 0.4)
  :config
  (which-key-mode t))

(use-package projectile
  :ensure t
  :init
  (setq projectile-sort-order 'recentf)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'hybrid)
  :config
  (projectile-mode t))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-parentheses
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package smartparens
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)) 

(use-package evil-cleverparens
  :ensure t
  :init
  (add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  :config
  (diff-hl-flydiff-mode))

;; (use-package exec-path-from-shell
;;   :ensure t
;;   ;; :if *is-a-mac*
;;   :config
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-env "PYTHONPATH")
;;   )

;; -- LANGUAGES -- 

(use-package elm-mode
  :ensure t)

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (define-key elpy-mode-map (kbd "s-b") 'elpy-goto-definition))

(use-package js-comint
  :ensure t
  :config 
  (setq js-comint-program-command "/Users/jordan/.asdf/shims/node"))



;; elpy-django-command 


;; -- KEYS --

;; TODO; fix this bullshit function
(defun jephron//switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "TAB" '(jephron//switch-to-previous-buffer
	   :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   ;; Buffers
   "bb"  '(counsel-ibuffer :which-key "buffers list")
   "bd"  '(kill-current-buffer :which-key "kill buffer")
   "bs"  '((lambda () (interactive) (switch-to-buffer "*scratch*"))
	   :which-key "open scratch buffer")

   ;; Projects
   "pp"  '(projectile-switch-project :which-key "switch project")
   "ph"  '(projectile-find-file :which-key "find files")

   ;; Search
   "sap" '(counsel-rg :which-key "find in path")

   ;; Toggles
   "Tf" '(toggle-frame-fullscreen :which-key "fullscreen")
   "Ts" '(counsel-load-theme :which-key "switch theme")

   ;; Others
   "at"  '(ansi-term :which-key "open terminal"))

  (general-define-key
   :keymaps 'org-mode-map
   "RET" 'org-return-indent
   )

  (general-define-key
   :states '(normal visual)
   :keymaps 'evil-cleverparens-mode-map
   "}" 'evil-forward-paragraph
   "{" 'evil-backward-paragraph
   ))

;; where are projects usually located?
(setq projectile-project-search-path
      (let*
	((work-paths (file-expand-wildcards "~/devel/work/*"))
	 (filtered-work-paths (seq-filter 'file-directory-p work-paths)))
        (append '("~/devel/personal")
		'("~/devel/sources")
		filtered-work-paths)))

;; enable C-w in ivy
(global-set-key (kbd "<C-tab>") 'jephron//switch-to-previous-buffer)

;; enable C-w in ivy
(define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)

;; make windows behave like tmux
(define-key evil-visual-state-map (kbd "C-a") 'evil-window-map)
(define-key evil-normal-state-map (kbd "C-a") 'evil-window-map)
(define-key evil-window-map (kbd "|") 'evil-window-vsplit)
(define-key evil-window-map (kbd "-") 'evil-window-split)
(define-key evil-window-map (kbd "L") 'evil-window-increase-width)
(define-key evil-window-map (kbd "H") 'evil-window-decrease-width)
(define-key evil-window-map (kbd "z") 'spacemacs/toggle-maximize-buffer)
(define-key evil-window-map (kbd "c") '(lambda () (interactive) ))
(define-key evil-window-map (kbd "x") 'evil-window-delete)

;; fix term
;; (define-key term-raw-map (kbd "C-a") 'evil-window-map)
;; (define-key term-raw-map (kbd "C-a C-a") 'term-send-raw)

;; Wrap point around like in tmux
(setq windmove-wrap-around t)

;; Incremental scrolling
(setq
 hscroll-step 1
 scroll-conservatively 1000)

;; Save on lose focus
(add-hook 'focus-out-hook
	  (lambda ()
	    (interactive)
	    (save-some-buffers t)))

(setq org-hierarchical-todo-statistics nil)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq ivy-initial-inputs-alist
      '((counsel-minor . "^+")
       (counsel-package . "^+")
       (org-refile . "^")
       (org-agenda-refile . "^")
       (org-capture-refile . "^")
       (counsel-M-x . "")
       (counsel-describe-function . "^")
       (counsel-describe-variable . "^")
       (counsel-org-capture . "^")
       (Man-completion-table . "^")
       (woman . "^")))

;; ==============================
;; BEGIN AUTOGENERATED CONTENT
;; ==============================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-environment nil)
 '(package-selected-packages
   (quote
    (js-comint elpy diff-hl git-gutter-fringe org-bullets evil-cleverparens evil-smartparens smartparens elm-mode ivy-hydra hydra highlight-parentheses rainbow-delimiters projectile evil-surround evil-visualstar amx evil-commentary counsel which-key ivy doom-themes use-package evil)))
 '(recentf-mode t)
 '(safe-local-variable-values
   (quote
    ((pyvenv-workon . truework-frontend-api)
     (eval progn
	   (setenv "DJANGO_SETTINGS_MODULE" "truework_frontend_api.settings")
	   (setenv "CLEARBIT_KEY" "sk_9307b239c6c998d63a805c1239a0afe4")
	   (setenv "CELERY_TASK_ALWAYS_EAGER" "False"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
