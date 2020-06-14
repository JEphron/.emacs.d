(use-package projectile
  :bind
  ("C-M-p" . projectile-switch-project)
  ("M-P" . projectile-find-file)
  ;; ("M-p" . projectile-find-file-in-known-projects)
  :config
  (setq projectile-completion-system 'ivy
        Projectile-sort-order 'recently-active
        projectile-indexing-method 'hybrid
        projectile-project-search-path '("~/eng/work" "~/eng/personal"))
  (projectile-mode 1))
