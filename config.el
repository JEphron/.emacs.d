;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here



;; GOOD STUFF
(map! :leader
      :desc "Eval expression" "SPC" #'execute-extended-command

      (:prefix-map ("p" . "project")
        :desc "Find file in project" "h" #'projectile-find-file))

(map!
 :nv "s-F" #'+default/search-project

 :nv "C-a" 'evil-window-map

 (:map evil-window-map
   "|" #'evil-window-vsplit
   "-" #'evil-window-hsplit
   "L" #'evil-window-increase-width
   "H" #'evil-window-decrease-width
   "z" #'spacemacs/toggle-maximize-buffer
   "x" #'evil-window-delete)
 :nvi "C-c C-f" #'+format/buffer
 :nvi "s-r" #'projectile-compile-project)

(setq compilation-read-command nil)

;; where are projects usually located?
(setq projectile-project-search-path
      (let*
          ((work-paths (file-expand-wildcards "~/devel/work/*"))
           (filtered-work-paths (seq-filter 'file-directory-p work-paths)))
        (append '("~/devel/personal")
                '("~/devel/sources")
                filtered-work-paths)))


;; docs! right now!
(setq which-key-idle-delay 0.33)

;; wrap point around like in tmux
(setq windmove-wrap-around t)

;; stolen from spacemacs (obviously)
;; from https://gist.github.com/3402786
(defun spacemacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))
