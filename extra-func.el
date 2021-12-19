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


;; Hydra
;; (after! hydra
;;   ;; Adjusted +hydra/window-nav with ivy and undo
;;   (defhydra jparcill/hydra-window-nav (:hint nil)
;;     "
;;           Split: _v_ert  _s_:horz
;;          Delete: _d_elete  _o_nly
;;   Move Window: _h_:left  _j_:down  _k_:up  _l_:right
;;         Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
;;            Undo: _u_ndo _U_:Redo
;;          Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
;;            Move: _a_:up  _z_:down
;; "
;;     ("z" scroll-up-line)
;;     ("a" scroll-down-line)
;;
;;     ("h" windmove-left)
;;     ("j" windmove-down)
;;     ("k" windmove-up)
;;     ("l" windmove-right)
;;
;;     ("p" previous-buffer)
;;     ("n" next-buffer)
;;     ("b" ivy-switch-buffer)
;;     ("f" find-file)
;;
;;     ("u" tab-bar-history-back)
;;     ("U" tab-bar-history-forward)
;;
;;     ("s" split-window-below)
;;     ("v" split-window-right)
;;
;;     ("d" delete-window)
;;     ("o" delete-other-windows)
;;
;;     ("H" hydra-move-splitter-left)
;;     ("J" hydra-move-splitter-down)
;;     ("K" hydra-move-splitter-up)
;;     ("L" hydra-move-splitter-right)
;;
;;     ("c" nil))
;;
;;   (defhydra jparcill/hydra-firefox ()
;;     ("mg" (browse-url-firefox "https://gmail.com") "gmail")
;;     ("mm" (browse-url-firefox "https://messenger.com") "messenger")
;;     ("mo" (browse-url-firefox "https://outlook.office.com") "outlook")
;;     ("el" (browse-url-firefox "https://lichess.org") "lichess")
;;     ("ey" (browse-url-firefox "https://youtube.com") "youtube")
;;     ("et" (browse-url-firefox "https://twitter.com") "twitter")
;;     ("K" (kill-matching-buffers "firefox") "kill all firefox")
;;     )
;;   )
