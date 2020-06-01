;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)

;; dont' indent using tabs
(setq-default indent-tabs-mode nil)

;; show recent files on startup
(setq initial-buffer-choice 'counsel-recentf)

;; don't create silly foo~ files
;; https://github.com/Silex/emacs-config/blob/master/config/emacs.el
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; auto-close parens and quotes
(electric-pair-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; disable pointless GUI things
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; make apropos search more things
(setq apropos-do-all t)

(global-display-line-numbers-mode 1)

(setq-default show-trailing-whitespace t)
;; https://github.com/lbrayner/dotemacs/blob/e15e0cdd19969f0f7a49a05a6f0814fc1132d616/init.el
(let ((no-show-trailing-space '(slime-repl-mode
                                help-mode
                                counsel-mode
                                eshell-mode
                                shell-mode
                                term-mode)))
  (cl-loop for mode in no-show-trailing-space
           do (let ((hook (concat (symbol-name mode) "-hook")))
                (add-hook (intern hook)
                          (lambda ()
                            (setq show-trailing-whitespace nil))))))

;; highlight matching paren
(show-paren-mode 1)

;; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

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


;; misc
(use-package eval-sexp-fu)
(use-package aggressive-indent)
