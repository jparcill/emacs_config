;;; org_conf.el -*- lexical-binding: t; -*-
(setq org-export-with-section-numbers nil)

(setq org-display-remote-inline-images 'download)
(setq org-latex-packages-alist '(
                                 ("margin=1in" "geometry" nil) ;; adjusting the margins of latex output
;;                                 ("" "minted")
                                 ))
;;(setq org-latex-listings 'minted)
;;(setq org-latex-minted-options '(("frame" "lines")
;;                                 ("linenos=true")))
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
;;       )
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5)) ;; making latex previews larger
(setq org-icalendar-use-scheduled '(event-if-todo, event-if-todo-not-done, event-if-not-todo, todo-start))
(setq org-icalendar-with-timestamps t)
(setq org-icalendar-combined-agenda-file (concat org-file-path "phone_folder/Tasker/combined.ics"))
(setq org-todo-keywords '((sequence
		           "TODO"
                           "PROJ"
		           "NEXT(n)"
		           "PROG(p!)"
		           "WAIT(w@/!)"
                           "SOMEDAY"
		           "|"
		           "DONE(d)"
		           "CANCEL(c@)"
		           "DELEGATED(@)"
                           )
		          (sequence
		           "IDEA"
		           "GOAL"
		           "|"
		           "DUD(@)")
                          (sequence
                           "RD"
                           "RDING"
                           "RDNOTE"
                           "TMPDROP"
                           "|"
                           "DROP"
                           "FNSHED"
                           )
                          ))


;; Org Superstar
;;
;;
;;
(setq org-superstar-remove-leading-stars t)
(setq org-superstar-headline-bullets-list '("◉" "○" "▷")) ;; '("🌹" "🌻" "🌼")

;;https://github.com/integral-dw/org-superstar-mode/blob/master/DEMO.org
;;
;;(set-face-attribute 'org-level-8 nil :weight 'bold :inherit 'default)
;;;; Low levels are unimportant => no scaling
;;(set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
;;(set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
;;(set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
;;(set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
;;;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
;;(set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.2) ;\large
;;(set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.44) ;\Large
;;(set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.728) ;\LARGE
;;;; Only use the first 4 styles and do not cycle.
;;(setq org-cycle-level-faces nil)
;;(setq org-n-level-faces 4)


;;(setq org-superstar-item-bullet-alist
;;      '((?+ . ?🌹)
;;        (?* . ?🌻)
;;        (?- . ?🌼)))

(setq org-capture-templates
      `(
        ("t" "Todo" entry (file ,(concat org-file-path "phone_folder/personal.org"))
         "* TODO %? \n%U" :empty-lines 1)
        ("d" "Todo deadline" entry (file ,(concat org-file-path "phone_folder/personal.org"))
         "* TODO %? \nDEADLINE: %^T\n%U" :empty-lines 1)
        ("w" "Wait deadline" entry (file ,(concat org-file-path "phone_folder/personal.org"))
         "* WAIT %? \nDEADLINE: %^T\n%U" :empty-lines 1)
        ("r" "Reading List" entry (file+olp ,(concat org-file-path "phone_folder/reading_list.org") "Catchall")
         "* RD %? \n%U" :empty-lines 1)
        ("s" "Someday" entry (file+olp ,(concat org-file-path "phone_folder/someday.org") "Catchall")
         "* SOMEDAY %? \n%U" :empty-lines 1)
        ("e" "Event" entry (file ,(concat org-file-path "phone_folder/personal.org"))
         "* %? \nSCHEDULED: %^T\n%U" :empty-lines 1)
        ("j" "Journal entry" entry (function jparcill/org-journal-find-location)
         "* NEXT %?\n%U" :empty-lines 1)
        ("k" "Journal sched entry" entry (function jparcill/org-journal-find-location)
         "* %? %^T\n%U" :empty-lines 1)
        ("m" "Morning Journal entry" entry (function jparcill/org-journal-find-location)
         "* Morning Entry
** Checklist
   - [[https://www.youtube.com/watch?v=GADW8Nlnc1s]]
   - [ ] Put on this week's album
   - [ ] make bed meditatively and clean (5 min)
   - [ ] Bruxism, Workout, Ergonomic stretch (5 min)
   - [ ] Yesterday's journal if not done (5 min)
   - [ ] Review whole month, and agenda TODOs (5 min)
   - [ ] Refresh phone orgzly
** Looking Forward To %?
** Day Plan
** Determinations :determ:" :empty-lines 1)
        ("n" "Night Journal entry" entry (function jparcill/org-journal-find-location)
         "* Today's Learnings\n* My Day\n%U")
        ))


;; Org Journal
(defun jparcill/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))


(set-face-attribute 'org-table nil :inherit 'fixed-pitch)

(set-face-attribute 'org-block nil :inherit 'fixed-pitch)

;; Refile taken from here: https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
;;(setq org-refile-targets '((nil :maxlevel . 9)
;;		           (org-agenda-files :maxlevel . 9)))
;;(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
;;(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
;;(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-pretty-entities t)

(setq org-hide-emphasis-markers t)
(setq org-fontify-whole-heading-line t)
(setq org-fontify-done-headline t)
(setq org-fontify-quote-and-verse-blocks t)
(setq org-tags-column 0)
(setq org-src-fontify-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

(setq org-log-done 'time)
