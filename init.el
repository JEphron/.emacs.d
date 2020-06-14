(load-theme 'wheatgrass t)

(defun je/load-directory (dir)
  (let* ((files (directory-files dir t "\\.el")))
    (mapc (lambda (file)
            (with-demoted-errors
                (load (file-name-sans-extension file) nil t)))
          files)))

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

;; load all the files in the .emacs.d/config directory
(je/load-directory
 (expand-file-name "config" (file-name-directory user-init-file)))


(defun je/splitwin ()
  (interactive)
  ;; Create new window right of the current one
  ;; Current window is 80 characters (columns) wide
  (split-window-right 80)
  ;; Go to next window
  (other-window 1)
  ;; Create new window below current one
  (split-window-below)
  ;; Start eshell in current window
  (eshell)
  ;; Go to previous window
  (other-window -1)
  ;; never open any buffer in window with shell
  (set-window-dedicated-p (nth 1 (window-list)) t))

;; customize
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
