;;; extra-func.el -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :defer t
  :config
  (pdf-loader-install)
  )


(use-package! org-noter
  :after pdf-tools
  :init
  (map! :map org-noter-doc-mode-map "i" #'org-noter-insert-note)
  (map! :map org-noter-notes-mode-map "C-c l i" #'org-noter-insert-note)
  )

(use-package! shrface
  :after eww
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t)
  (add-hook! 'eww-after-render-hook 'shrface-mode)
  )

(use-package! org-download
  :defer t
  :init
  ;; Org download
  (setq-default org-download-image-dir (concat org-file-path "img/"))
  (setq-default org-download-method 'directory)
  (setq-default org-download-screenshot-method "scrot")
  :config
  (org-download-enable)
  )

(add-hook! 'dired-mode-hook 'org-download-enable)


(use-package! org-roam
  :defer t
  :init
  (setq org-roam-directory org-file-path)
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-node-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-node-find
        :desc "Org-Roam-Buffer" "r" #'org-roam)
  )
