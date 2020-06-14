(use-package all-the-icons)

(defun je/neotree-window-focused-p ()
  (eq (selected-window) neo-global--window))

(defun je/neotree-focus-or-toggle ()
  (interactive)
  (if (and (neo-global--window-exists-p) (je/neotree-window-focused-p))
        (neotree-hide)
      (neotree-show)))

(defun je/neotree-find-mru ()
  (interactive)
  (if (je/neotree-window-focused-p)
      (let ((mru-buffer (window-buffer (get-mru-window))))
         (neotree-find (buffer-file-name mru-buffer)))
      (neotree-find)))

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
    (evil-define-key 'normal neotree-mode-map (kbd "t") 'je/neotree-find-mru)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  )
