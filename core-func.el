;;; core-func.el -*- lexical-binding: t; -*-

(after! org (load! "org-conf.el"))

(defun jparcill/after-org-mode-load ()
  (interactive)
  (setq olivetti-body-width 0.8)
  (olivetti-mode)
  )

(add-hook! 'org-mode-hook 'jparcill/after-org-mode-load)

(use-package! org-journal
  :defer t
  :init
  ;; org journal
  (setq org-journal-dir (concat org-file-path "journal/2022/"))
  (setq org-journal-file-type 'daily)
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-date-format "%A, %B %d %Y")
  (setq org-extend-today-until 4)
  :config
  (setq org-journal-carryover-items "")
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

(use-package! org-agenda
  :defer t
  :init

  (setq org-agenda-files (list
                          (concat org-file-path "phone_folder/")
                          ;;(concat org-file-path "taxes.org")
                          ;;(concat org-file-path "reflections/2021_refl.org")
                          ;;(concat org-file-path "projects/2021/")
                          ))

  :config
  (setq org-habit-show-habits-only-for-today t)
  (setq org-agenda-include-deadlines t)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-dim-blocked-tasks "invisible")

  ;; org agenda
  ;;(setq org-agenda-time-grid
  ;;      (quote
  ;;       ((daily today remove-match)
  ;;        (700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300)
  ;;        "......" "----------------")))
  )


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
                                                       )))))
                                      )

                                     ("w" "Someday and Idea"
                                      ((alltodo "" ((org-agenda-overriding-header "")
                                                    (org-super-agenda-groups
                                                     '(
                                                       (:todo "IDEA")
                                                       (:todo "SOMEDAY")
                                                       (:discard (:not "IDEA"))
                                                       )
                                                     )))))

                                     ("R" "Today's" ((agenda "" ((org-agenda-span 'day)
                                                                 (org-agenda-start-day "+0d")
                                                                 (org-agenda-overriding-header "")
                                                                 (org-super-agenda-groups
                                                                  '((:name "Today"
                                                                     :date today
                                                                     :scheduled today
                                                                     :todo "TODAY"
                                                                     :discard (:not (:deadline today))))))))
                                      nil (concat org-file-path "phone_folder/Tasker/today.txt"))
                                     ))
  ;;:config
  (org-super-agenda-mode)
  )
