(use-package all-the-icons)

(defun je/neotree-focus-or-toggle ()
  (interactive)
  (let ((neo-window-focused (eq (selected-window) neo-global--window)))
    (if (and (neo-global--window-exists-p) neo-window-focused)
        (neotree-hide)
      (neotree-show))))

(use-package neotree
  :after (evil)
  :bind
  ([f8] . 'je/neotree-focus-or-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  )
