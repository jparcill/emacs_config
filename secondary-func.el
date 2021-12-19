;;; secondary-func.el -*- lexical-binding: t; -*-

(after! ranger
  (setq ranger-show-hidden t)
  )

(use-package! ox-hugo
  :after ox
  )

(use-package! elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))
  )

(use-package! vterm
  :defer t
  :init
  (map! :map vterm-mode-map "C-c C-\\" #'vterm-send-C-c)
  )

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
