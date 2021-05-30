;;; machine_specific/linux_pc.el -*- lexical-binding: t; -*-
;; Font
(setq doom-font (font-spec :family "Julia Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Libre Baskerville" :size 19))



(add-to-list 'exec-path "/home/jparcill/.local/bin")
;; EXWM
(use-package! exwm
  :config
  ;; These bindings mimic some of the main bindings for doom emacs but with super instead.
  (exwm-input-set-key (kbd "s-;") #'counsel-linux-app)
  ;; have to do these twice because sometimes I press the keys too fast
  (exwm-input-set-key (kbd "s-w s-h") #'evil-window-left)
  (exwm-input-set-key (kbd "s-w h") #'evil-window-left)
  (exwm-input-set-key (kbd "s-w s-l") #'evil-window-right)
  (exwm-input-set-key (kbd "s-w l") #'evil-window-right)
  (exwm-input-set-key (kbd "s-w s-j") #'evil-window-down)
  (exwm-input-set-key (kbd "s-w j") #'evil-window-down)
  (exwm-input-set-key (kbd "s-w s-k") #'evil-window-up)
  (exwm-input-set-key (kbd "s-w k") #'evil-window-up)

  (exwm-input-set-key (kbd "s-w s") #'evil-window-split)
  (exwm-input-set-key (kbd "s-w v") #'evil-window-vsplit)
  (exwm-input-set-key (kbd "s-w u") #'jparcill/hydra-window-undo/body)

  (exwm-input-set-key (kbd "s-w m m") #'doom/window-maximize-buffer)
  (exwm-input-set-key (kbd "s-w d") #'evil-window-delete)
  (exwm-input-set-key (kbd "s-b b") #'+ivy/switch-buffer)
  (exwm-input-set-key (kbd "s-b k") #'kill-current-buffer)
  (exwm-input-set-key (kbd "s-b i") #'ibuffer)
  (exwm-input-set-key (kbd "s-x") #'doom/open-scratch-buffer)

  (exwm-input-set-key (kbd "s-o t") #'+shell/toggle)
  (exwm-input-set-key (kbd "s-f f") #'counsel-find-file)
  (exwm-input-set-key (kbd "s-f r") #'counsel-recentf)

  (exwm-input-set-key (kbd "<s-tab> ]") #'tab-bar-switch-to-next-tab)
  (exwm-input-set-key (kbd "<s-tab> [") #'tab-bar-switch-to-prev-tab)
  (exwm-input-set-key (kbd "<s-tab> n") #'tab-bar-new-tab)
  (exwm-input-set-key (kbd "<s-tab> d") #'tab-bar-close-tab)
  (exwm-input-set-key (kbd "<s-tab> r") #'tab-bar-rename-tab)
  (exwm-input-set-key (kbd "s-m") 'jparcill/hydra-window-nav/body)
  (exwm-input-set-key (kbd "s-s") 'jparcill/hydra-firefox/body)


  (defun jparcill/tmp-screenshot ()
      (interactive)
      (start-process-shell-command "screenshot" "screenshot"  (format-time-string "scrot -s /tmp/%Y-%m-%d-%H:%M:%S.png"))
    )
  ;; C-c l will be for linux shortcuts
  (exwm-input-set-key (kbd "C-c g") 'counsel-search)
  (exwm-input-set-key (kbd "C-c l s") 'jparcill/tmp-screenshot)


  (load! "~/.doom.d/machine_specific/exwm-conf.el")
  (exwm-config-personal)
  (exwm-enable)
  )

(use-package! exwm-firefox
  :after exwm
  :config
  (exwm-firefox-mode)
  )

(use-package! exwm-edit
  :after exwm
  )

(use-package! mathpix
  :config
  (jparcill/mathpix-settings)
  (setq mathpix-screenshot-method "scrot -s %s")
  )


(add-hook! 'exwm-edit-compose-hook (lambda () (funcall 'markdown-mode)))

(setq async-shell-command-buffer 'new-buffer)


;;(start-process-shell-command "jparcill-startup" "jparcill-startup" "sh ~/.doom.d/machine_specific/startup.sh")

;;(add-hook! 'exwm-exit-hook (lambda () (kill-buffer "jparcill-startup")))
