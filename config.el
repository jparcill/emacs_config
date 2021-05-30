;;; .doom.d/config.el -*- lexical-biding: t; -*-

;; DATE
;;[2019-11-26 Tue]
;; My own config functions

;; Device Variables
;; ~~~~~~~~~~~~~~~~

(setq org-file-path "~/Sync/Org/")
(setq work-path "~/Org/")

;; Private
;; --------
(load! "private.el")

;; Machine dependent-code
;; ----------------------

;; Linux Specific
(cond ((equal (string-trim (shell-command-to-string "hostname"))  "jparcill")
       (load! "./machine_specific/linux_pc.el"))
      ((equal (string-trim (shell-command-to-string "hostname")) "localhost")
       (load! "./machine_specific/phone.el"))
  )


;; Aesthetics
;; -----------
;; Linum
(setq display-line-numbers-type 'relative)

;;theme
(setq doom-theme 'doom-rouge)

;; Taken from https://tecosaur.github.io/emacs-config/config.html
(defun jparcill/doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'jparcill/doom-modeline-conditional-buffer-encoding)



;; Packages
;; --------

(after! org (load! "org-conf.el"))

(defun jparcill/after-org-mode-load ()
  (interactive)
  (setq olivetti-body-width 0.8)
  (olivetti-mode)
  )

(add-hook! 'org-mode-hook 'jparcill/after-org-mode-load)


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
  :init
  (map! :map org-noter-doc-mode-map "i" #'org-noter-insert-note)
  (map! :map org-noter-notes-mode-map "C-c l i" #'org-noter-insert-note)
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
        `(
          ("PROJ"  . ,(face-foreground 'error))
          ("SOMEDAY"  . ,(face-foreground 'warning))
          ("TODO"  . ,(face-foreground 'warning))
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
  (setq org-journal-dir (concat org-file-path "journal/2021/"))
  (setq org-journal-file-type 'daily)
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-date-format "%A, %B %d %Y")
  (setq org-extend-today-until 4)
  :config
  (setq org-journal-carryover-items "")
  )

(use-package! org-agenda
  :defer
  :init
  (setq org-agenda-files (list
                          (concat org-file-path "projects.org")
                          (concat org-file-path "monthly_habits.org")
                          (concat org-file-path "quarterly_habits.org")
                          (concat org-file-path "personal.org")
                          (concat org-file-path "taxes.org")
                          (concat org-file-path "birthdays_and_important_days.org")
                          (concat org-file-path "reading_list.org")
                          (concat org-file-path "school.org")
                          (concat org-file-path "daily_habits.org")
                          (concat org-file-path "weekly_habits.org")
                          (concat org-file-path "reflections/2021_refl.org")
                          (concat org-file-path "someday.org")
                          work-path
                          (concat org-file-path "projects/2021/")
                          org-journal-dir))

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

  (setq org-agenda-custom-commands '(
                                     ("r" "Main View"
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
                                                     '(
                                                       (:discard (:habit))
                                                       (:todo "PROJ")
                                                       (:todo "PROG")
                                                       (:todo "NEXT")
                                                       (:todo "WAIT")
                                                       (:todo "RDNOTE")
                                                       (:name "Important" :priority "A")
                                                       (:todo "TODO")
                                                       (:todo "GOAL")
                                                       (:discard (:todo "IDEA"))
                                                       (:discard (:todo "RD"))
                                                       (:discard (:todo "TMPDROP"))
                                                       (:discard (:todo "SOMEDAY"))
                                                       ))))))

                                     ("w" "Someday and Idea"
                                      ((alltodo "" ((org-agenda-overriding-header "")
                                                    (org-super-agenda-groups
                                                     '(
                                                       (:todo "IDEA")
                                                       (:todo "SOMEDAY")
                                                       (:discard (:not "IDEA"))
                                                       )
                                                     )))))))


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
    ("u" winner-undo "undo")
    ("U" winner-redo "redo")
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

  (defhydra jparcill/hydra-firefox ()
    ("mg" (browse-url-firefox "https://gmail.com") "gmail")
    ("mm" (browse-url-firefox "https://messenger.com") "messenger")
    ("mo" (browse-url-firefox "https://outlook.office.com") "outlook")
    ("el" (browse-url-firefox "https://lichess.org") "lichess")
    ("ey" (browse-url-firefox "https://youtube.com") "youtube")
    ("et" (browse-url-firefox "https://twitter.com") "twitter")
    ("K" (kill-matching-buffers "firefox") "kill all firefox")
    )
  )


;; Custom Functions
;;

;;

;; Function for clicking on the nearest file link above my cursor
;; Useful for me personally as I leave file links around in my org file
(defun jparcill/click-on-file-above ()
  (interactive)
  (progn (search-backward "dir: [[file:")
         (+org/dwim-at-point)
         )
  )
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


;; https://github.com/goktug97/yet-another-spotify-lyrics
(defun jparcill/spotify-lyrics ()
  (interactive)
  (let ((string (shell-command-to-string "~/.local/bin/spotify-lyrics-once")))
    (get-buffer-create "lyrics-buffer")
    (switch-to-buffer-other-window "lyrics-buffer")
    (with-current-buffer "lyrics-buffer"
      (goto-char (point-max))
      (erase-buffer)
      (insert string)
      (goto-line 1))))

;; Function to export a temporary pdf
(defun jparcill/tmp-org-to-pdf ()
  (interactive)
  (let ((new-file-name (concat "/tmp/" (buffer-name) ".pdf")))
    (start-process-shell-command
     (concat (buffer-name) "-tmp-pdf")
     (concat (buffer-name) "-tmp-pdf")
     (concat "pandoc -o " new-file-name " " (buffer-file-name) " --pdf-engine=xelatex"))
    (sleep-for 1)
    (find-file new-file-name)
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
     (concat org-journal-dir (concat date ".org"))
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
(global-set-key (kbd "C-c s l") 'counsel-spotify-search-playlist)

(evil-define-key 'normal org-mode-map
  (kbd "SPC m ;") 'org-babel-execute-src-block
  (kbd "SPC m :") 'org-babel-execute-subtree)

(evil-define-key 'normal hydra-curr-map
  (kbd "ESC") 'hydra-keyboard-quit)

(evil-define-key 'normal hydra-base-map
  (kbd "ESC") 'hydra-keyboard-quit)


(evil-define-key* 'normal 'global
  (kbd "SPC g C-g") #'counsel-git-grep
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
