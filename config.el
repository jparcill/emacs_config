;;; .doom.d/config.el -*- lexical-biding: t; -*-

;; DATE
;;[2019-11-26 Tue]
;; My own config functions

;; Device Variables
;; ~~~~~~~~~~~~~~~~

(setq org-file-path "~/Sync/Org/")
(setq work-path "~/Org/")


;; Machine dependent-code
;; ----------------------

;; Linux Specific
(if (equal (string-trim (shell-command-to-string "hostname"))  "jparcill")
    (load! "./machine_specific/linux_pc.el")
  )


;; Aesthetics
;; -----------
;; Font
(setq doom-font (font-spec :family "Julia Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Raleway" :size 16))

;; Linum
(setq display-line-numbers-type 'relative)

;;theme
(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")
(setq doom-theme 'doom-nord-light)
;; nightmode theme
(run-at-time "21:00" nil (lambda ()
                           (progn (load-theme 'doom-rouge)
                                  (start-process-shell-command "redshift" "redshift" "redshift -x; redshift -O 1700")
                                  )))


;; Taken from https://tecosaur.github.io/emacs-config/config.html
(defun jparcill/doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'jparcill/doom-modeline-conditional-buffer-encoding)



;; Packages
;; --------
(load! "private.el")

(after! org (load! "org-conf.el"))
;; Org mode from
;; https://emacs.christianbaeuerlein.com/my-org-config.html
(defun jparcill/after-org-mode-load ()
  (olivetti-mode)
  (setq olivetti-body-width 0.7)
  (display-line-numbers-mode -1)
  )

(add-hook! 'org-mode-hook 'jparcill/after-org-mode-load)

(remove-hook! 'org-mode-hook #'flyspell-mode)


(after! ranger
  (setq ranger-show-hidden t)
  )

(use-package! pdf-tools
  :defer
  :config
  (pdf-loader-install)
  )

(use-package! nov
  :defer
  )

(use-package! counsel-spotify
  :defer
  :config
  (jparcill/counsel-spotify-settings)
  )


(use-package! org-noter
  :after pdf-tools
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

(use-package! shrface
  :after eww
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t)
  (add-hook! 'eww-after-render-hook 'shrface-mode)
  )

(use-package! deft
  :hook deft-mode-hook
  :init
  (setq deft-directory "~/Sync/Org/")
  (setq deft-recursive t)
  )

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("PROG" . ,(face-foreground 'error))
          ("NEXT" . ,(face-foreground 'error))
          ("WAIT" . ,(face-foreground 'warning))
          ("CANCEL" . ,(face-foreground 'error))
          ("DELEGATED" . ,(face-foreground 'error))
          ("IDEA" . ,(face-foreground 'warning))
          ("GOAL" . ,(face-foreground 'warning))
          ("DUD" . ,(face-foreground 'error))
          ("RD" . ,(face-foreground 'warning))
          ("RDING" . ,(face-foreground 'warning))
          ("RDNOTE" . ,(face-foreground 'warning))
          ("TMPDROP" . ,(face-foreground 'warning))
          ("DROP" . ,(face-foreground 'error))
          ("FNSHED" . ,(face-foreground 'success))
          ("DONE"  . ,(face-foreground 'success))))
  )

(use-package! tab-bar
  :config
  (tab-bar-mode)
  (tab-bar-history-mode)
  )

(use-package! mathpix
  :config
  (jparcill/mathpix-settings)
  (setq mathpix-screenshot-method "scrot -s %s")
  )

(use-package! calfw-org
  :after calfw
  )

(use-package! vterm
  :defer
  :init
  (map! :map vterm-mode-map "C-c C-\\" #'vterm-send-C-c)
  )

(use-package! org-download
  :defer
  :init
  ;; Org download
  (setq-default org-download-image-dir (concat org-file-path "img/"))
  (setq-default org-download-method 'directory)
  (setq-default org-download-screenshot-method "scrot")
  :config
  (org-download-enable)
  )

(add-hook! 'dired-mode-hook 'org-download-enable)

(use-package! org-journal
  :defer
  :init
  ;; org journal
  (setq org-journal-dir (concat org-file-path "journal/"))
  (setq org-journal-file-type 'daily)
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-date-format "%A, %B %d %Y")
  (setq org-extend-today-until 4)
  :config
  (setq org-journal-carryover-items "")
  )

(use-package! org-krita
  :defer
  :config
  (setq org-krita-template-path "/home/jparcill/.doom.d/custom_packages/org-krita-0.1.1/resources/template.kra")
  )

(use-package! org-agenda
  :defer
  :init
  (setq org-agenda-files (list
                          (concat org-file-path "emacs_todos.org")
                          (concat org-file-path "linux_todos.org")
                          (concat org-file-path "projects.org")
                          (concat org-file-path "reading_list.org")
                          (concat org-file-path "monthly_habits.org")
                          (concat org-file-path "quarterly_habits.org")
                          (concat org-file-path "personal.org")
                          (concat org-file-path "daily_habits.org")
                          (concat org-file-path "weekly_habits.org")
                          "/home/jparcill/Sync/Org/20200908101858-pmath330.org"
                          "/home/jparcill/Sync/Org/20200908101947-pmath340_elementary_number_theory.org"
                          "/home/jparcill/Sync/Org/20200908130923-pmath331_applied_real_analysis.org"
                          "/home/jparcill/Sync/Org/20200908102038-stat431.org"
                          work-path
                          (concat org-file-path "journal/")))

  :config
  (setq org-habit-show-habits-only-for-today t)
  ;; Org Agenda Files

  ;; org agenda
  (setq org-agenda-time-grid
        (quote
         ((daily today remove-match)
          (700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300)
          "......" "----------------")))
  )
;; org-super-agenda
;; https://www.reddit.com/r/orgmode/comments/erdb40/doomemacs_can_not_make_orgsuperagenda_work/
(use-package! org-super-agenda
  :after org-agenda
  :init
  ;; for some reason org-agenda evil bindings were being weird with j and k
  (map! :map org-agenda-keymap "j" #'org-agenda-next-line)
  (map! :map org-agenda-mode-map "j" #'org-agenda-next-line)
  (map! :map org-super-agenda-header-map "j" #'org-agenda-next-line)
  (map! :map org-agenda-keymap "k" #'org-agenda-previous-line)
  (map! :map org-agenda-mode-map "k" #'org-agenda-previous-line)
  (map! :map org-super-agenda-header-map "k" #'org-agenda-previous-line)
  (map! :map org-super-agenda-header-map "k" #'org-agenda-previous-line)
  (map! :map org-super-agenda-header-map "k" #'org-agenda-previous-line)

  (setq org-agenda-custom-commands '(("z" "Super work view"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-agenda-start-day "+0d")
                                                   (org-agenda-overriding-header "")
                                                   (org-super-agenda-groups
                                                    '((:name "Today"
                                                       :time-grid t
                                                       :date today
                                                       :order 1
                                                       :scheduled today
                                                       :todo "TODAY")))))
                                       (alltodo "" ((org-agenda-overriding-header "")
                                                    (org-super-agenda-groups
                                                     '((:discard (:todo "RD"))
                                                       (:discard (:todo "TMPDROP"))
                                                       (:todo "PROG")
                                                       (:todo "NEXT")
                                                       (:name "Important" :priority "A")
                                                       (:todo "WAIT")
                                                       (:name "Work" :tag "work")
                                                       (:name "School" :tag "school")
                                                       (:name "Hobby" :tag "hobby")
                                                       (:todo "RDNOTE")
                                                       (:todo "GOAL")
                                                       (:discard (:habit))
                                                       ))))))
                                     ("r" "Super relaxed view"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-agenda-start-day "+0d")
                                                   (org-agenda-overriding-header "")
                                                   (org-super-agenda-groups
                                                    '((:name "Today"
                                                       :time-grid t
                                                       :date today
                                                       :order 1
                                                       :scheduled today
                                                       :todo "TODAY")))))
                                       (alltodo "" ((org-agenda-overriding-header "")
                                                    (org-super-agenda-groups
                                                     '((:todo "PROG")
                                                       (:todo "NEXT")
                                                       (:name "Important" :priority "A")
                                                       (:name "Hobby" :tag "hobby")
                                                       (:todo "RDING")
                                                       (:todo "RDNOTE")
                                                       (:todo "GOAL")
                                                       (:todo "WAIT")
                                                       (:habit)
                                                       (:name "Other" :not (:tag "work"))
                                                       ))))))
                                     ("d" "Done View"
                                      ((todo "DONE" ((org-agenda-view-columns-initially t)))))))

  :config
  (org-super-agenda-mode)
  )



;; Org Roam
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/Sync/Org/")
  (setq org-roam-buffer-width 0.1)
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer" "r" #'org-roam)
  )


;; Attempt to remove lag
(setq key-chord-two-keys-delay 0.7)


;; Hydra
(after! hydra
  (defhydra jparcill/hydra-window-undo ()
    "undo"
    ("u" tab-bar-history-back "undo")
    ("U" tab-bar-history-forward "redo")
    )


  ;; Adjusted +hydra/window-nav with ivy and undo
  (defhydra jparcill/hydra-window-nav (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _d_elete  _o_nly
  Move Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
           Undo: _u_ndo _U_:Redo
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down
"
    ("z" scroll-up-line)
    ("a" scroll-down-line)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ivy-switch-buffer)
    ("f" find-file)

    ("u" tab-bar-history-back)
    ("U" tab-bar-history-forward)

    ("s" split-window-below)
    ("v" split-window-right)

    ("d" delete-window)
    ("o" delete-other-windows)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("c" nil))
  )


;; Custom Functions
;;
;; Uploading Images to Journal
(defun jparcill/img-path-string (date)
  (interactive)
  (mapconcat (function (lambda (x) (concat (concat "\[\[" x) "\]\]\n")))
             (seq-filter (function (lambda (x) (cl-search date x)))
                         (directory-files-recursively "~/mobile_images/"
                                                      "\\`[^.].*\\.jpg\\'"
                                                      )
                         )
             ""
             )
  )

(defun jparcill/upload-imgs-to-journal ()
  "I use this function to attach images to the journal for the date that I choose"
  (interactive)
  (let ((date
         (replace-regexp-in-string "[^[:digit:]]" "" (org-read-date))))

    (append-to-file
     (concat "* Images\n" (jparcill/img-path-string date))
     nil
     (concat org-file-path (concat "journal/" (concat date ".org")))
     )
    )
  )

(defun jparcill/open-with-xournal ()
  (interactive)
  (start-process-shell-command (f-this-file) (f-this-file) (concat "xournalpp " (f-this-file))))

;; Keybindings
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(global-set-key (kbd "C-c b") 'browse-kill-ring)
(global-set-key (kbd "C-c c") '=calendar)
(global-set-key (kbd "C-c l m") 'mathpix-screenshot)

(if (not (equal (string-trim (shell-command-to-string "hostname"))  "jparcill"))
    (global-set-key (kbd "C-c g") 'eww))

;; Input images to org journal
(global-set-key (kbd "C-c i") 'jparcill/upload-imgs-to-journal)

(global-set-key (kbd "C-c s SPC") 'counsel-spotify-toggle-play-pause)
(global-set-key (kbd "C-c s n") 'counsel-spotify-next)
(global-set-key (kbd "C-c s p") 'counsel-spotify-previous)
(global-set-key (kbd "C-c s /") 'counsel-spotify-search-album)
(global-set-key (kbd "C-c s ?") 'counsel-spotify-search-track)

(evil-define-key 'normal org-mode-map
  (kbd "SPC m ;") 'org-babel-execute-src-block
  (kbd "SPC m :") 'org-babel-execute-subtree)

(evil-define-key 'normal hydra-curr-map
  (kbd "ESC") 'hydra-keyboard-quit)

(evil-define-key 'normal hydra-base-map
  (kbd "ESC") 'hydra-keyboard-quit)


;; Replacing doom's default: workspaces with tab-bar from Emacs 27.1
(evil-define-key* 'normal 'global
  (kbd "SPC g C-g") #'counsel-git-grep
  (kbd "SPC TAB \]") #'tab-bar-switch-to-next-tab
  (kbd "SPC TAB \[") #'tab-bar-switch-to-prev-tab
  (kbd "SPC TAB n") #'tab-bar-new-tab
  (kbd "SPC TAB d") #'tab-bar-close-tab
  (kbd "SPC TAB r") #'tab-bar-rename-tab
  )

(map! :leader
      :prefix "w"
      :desc "Tab Bar History Hydra" "u" #'jparcill/hydra-window-undo/body
      )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#FFFBEA" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(custom-safe-themes
   (quote
    ("99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "a92e9da0fab90cbec4af4a2035602208cebf3d071ea547157b2bfc5d9bd4d48d" "632694fd8a835e85bcc8b7bb5c1df1a0164689bc6009864faed38a9142b97057" default)))
 '(fci-rule-color "#D6D6D6")
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(objed-cursor-color "#dc322f")
 '(org-modules (quote (org-habit)))
 '(package-selected-packages (quote (htmlize ox-hugo exwm)))
 '(pdf-view-midnight-colors (cons "#556b72" "#FDF6E3"))
 '(rustic-ansi-faces
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(vc-annotate-background "#FDF6E3")
 '(vc-annotate-color-map
   (list
    (cons 20 "#859900")
    (cons 40 "#959300")
    (cons 60 "#a58e00")
    (cons 80 "#b58900")
    (cons 100 "#bc7407")
    (cons 120 "#c35f0e")
    (cons 140 "#cb4b16")
    (cons 160 "#cd4439")
    (cons 180 "#d03d5d")
    (cons 200 "#d33682")
    (cons 220 "#d63466")
    (cons 240 "#d9334a")
    (cons 260 "#dc322f")
    (cons 280 "#dd5c56")
    (cons 300 "#de867e")
    (cons 320 "#dfb0a5")
    (cons 340 "#D6D6D6")
    (cons 360 "#D6D6D6")))
 '(vc-annotate-very-old-color nil))
