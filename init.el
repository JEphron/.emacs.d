(load-theme 'wheatgrass)

;; (load (expand-file-name "~/.roswell/helper.el"))
;; (setq inferior-lisp-program "ros -Q run")

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

;; don't indent using tabs
(setq-default indent-tabs-mode nil)

;; show recent files on startup
(setq initial-buffer-choice 'counsel-recentf)

;; sane defaults
(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; macos - avoid horrible fullscreen animations
(setq ns-use-native-fullscreen nil)

;; try to remember what things were open
(desktop-save-mode 1)

;; make apropos search more things
(setq apropos-do-all t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; sane scrolling?
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; don't create silly foo~ files
;; https://github.com/Silex/emacs-config/blob/master/config/emacs.el
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default show-trailing-whitespace t)

(use-package doom-themes)

 ;; show key info
 (use-package which-key
   :config
   (which-key-mode))

 (use-package company
   :config
   (setq company-minimum-prefix-length 1
         company-idle-delay 0.0)
   :hook
   (prog-mode-hook . company-mode))

 ;; ----------
 ;; -- evil --

 (use-package evil
   :init
   (setq evil-want-keybinding nil)
   (setq evil-respect-visual-line-mode t)
   :config
   (evil-mode 1)
   (defalias #'forward-evil-word #'forward-evil-symbol)
   ;; make evil-search-word look for symbol rather than word boundaries
   (setq-default evil-symbol-word-search t))

 (use-package evil-surround
   :after (evil)
   :config
   (global-evil-surround-mode t))

 (use-package evil-commentary
   :after (evil)
   :config (evil-commentary-mode))

 (use-package evil-collection
   :config (evil-collection-init))

 (use-package ranger
   :config
   (ranger-override-dired-mode t))

 ;; ---------
 ;; -- ivy --

 (use-package counsel :demand
   ;; fuzzy searching thing
   :bind (("s-A" . counsel-M-x)
 	 ; ("/" . swiper) ; todo: only in normal mode
          ("s-F" . counsel-rg)
 	 )
   :config
   (setq ivy-initial-inputs-alist nil
         ivy-use-virtual-buffers t
         ivy-count-format "%d/%d ")
   (ivy-mode 1)
   (counsel-mode 1))

 (use-package ivy-prescient
   :config
   (ivy-prescient-mode 1))

 ;; ------------------
 ;; -- projectile --

 (use-package projectile
   :bind (:map
          projectile-mode-map
          ("C-c p" . projectile-command-map)
   :config
   (projectile-mode 1))
 ;; ------------------
 ;; -- lang:general --


 (use-package lsp-mode
   :hook
   ((haskell-mode . lsp)
    (rust-mode . lsp)
    (lsp-mode . lsp-enable-which-key-integration))
   :commands lsp)

 (use-package lsp-ui
   :commands lsp-ui-mode
   :config
   (setq lsp-ui-doc-enable nil)
   (setq lsp-ui-doc-position 'at-point)
   :bind
   (:map lsp-mode-map
         ("C-j" . lsp-ui-doc-glance)))

 (use-package lsp-ivy
   :commands lsp-ivy-workspace-symbol)

 (use-package flycheck)

 (use-package reformatter)

 ;;(use-package git-gutter-diff
 ;;  :commands je/foo
 ;;  :load-path "~/dev/oss/git-gutter-diff-el")

 ;; ------------------
 ;; -- prose --
 (use-package writeroom-mode)

 ;; ------------------
 ;; -- lang:factor --

 (use-package fuel)
 (setq fuel-listener-factor-binary "~/downloads/factor/factor")
 (setq fuel-listener-factor-image "~/downloads/factor/factor.image")

 ;; ------------------
 ;; -- lang:rust --

 (use-package rust-mode
   :config
   (setq rust-format-on-save t)
   :bind (("C-c C-c" . rust-run))
   :hook
   (rust-mode-hook . (lambda () (setq indent-tabs-mode nil))))


 ;; ------------------
 ;; -- lang:haskell --

 (use-package lsp-haskell)

 (use-package haskell-mode
   :after (reformatter)
   :config
   (reformatter-define ormolu-format
                       :program "ormolu"
                       :group 'haskell
                       :lighter " Ormolu")
   :hook
   (haskell-mode . ormolu-format-on-save-mode))

 ;; ---------------
 ;; -- lang:lisp --

 (use-package lispy
   :config
   (define-key lispy-mode-map-lispy (kbd "\"") 'lispy-doublequote)
   (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
   (add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
   (setq lispy-close-quotes-at-end-p t))

 (add-to-list 'load-path "~/sources/sly")
 (require 'sly-autoloads)
 (setq inferior-lisp-program "/opt/sbcl/bin/sbcl")

 ;; ------------
 ;; -- lang:nim --
 (use-package nim-mode)

 ;; ------------
 ;; -- lang:d --
 (use-package d-mode)

 ;; ------------
 ;; -- lang:glsl --
 (use-package glsl-mode)

 ;; ------------
 ;; -- visual --

 ;; (use-package rainbow-blocks
 ;;   :hook
 ;;   (emacs-lisp-mode . rainbow-blocks-mode)
 ;;   (listp-mode . rainbow-blocks-mode))


 ;; --------------
 ;; -- fns:misc --

 ;; easily change font size
 (defun je/change-font-scale (amnt)
   (set-face-attribute 'default nil :height
                     (+ (face-attribute 'default :height) amnt)))

 (defun je/increase-font-scale () (interactive)
   (je/change-font-scale 10))

 (defun je/decrease-font-scale () (interactive)
   (je/change-font-scale -10))

 (bind-key "<C-S-prior>" 'je/increase-font-scale)
 (bind-key "<C-S-next>" 'je/decrease-font-scale)
 (bind-key "C-M-r" 'eval-buffer)

 ;; set and save theme
 (defun color-scheme ()
   (interactive)
   (customize-set-variable custom-enabled-themes (counsel-load-theme)))

 (custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
    [default default default italic underline success warning error])
  '(ansi-color-names-vector
    ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
  '(lispy-close-quotes-at-end-p t)
  '(package-selected-packages
    '(projectile writeroom-mode olivetti fuel nim-mode company which-key flycheck lsp-haskell lsp-ui lsp-mode reformatter evil-commentary haskell-mode evil-collection ranger evil ivy-prescient doom-themes emacs-doom-themes rainbow-delimiters lispy use-package)))
 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
