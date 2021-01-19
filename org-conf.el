;;; org_conf.el -*- lexical-binding: t; -*-
(setq org-export-with-section-numbers nil)
(setq org-agenda-include-deadlines t)
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-latex-packages-alist '(("margin=1in" "geometry" nil)))
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



(setq org-capture-templates
      `(
        ("t" "Todo" entry (file ,(concat org-file-path "personal.org"))
         "* TODO %? \n%U" :empty-lines 1)
        ("d" "Todo deadline" entry (file ,(concat org-file-path "personal.org"))
         "* TODO %? \nDEADLINE: %^T\n%U" :empty-lines 1)
        ("w" "Wait deadline" entry (file ,(concat org-file-path "personal.org"))
         "* WAIT %? \nDEADLINE: %^T\n%U" :empty-lines 1)
        ("r" "Reading List" entry (file+olp ,(concat org-file-path "reading_list.org") "Catchall")
         "* RD %? \n%U" :empty-lines 1)
        ("s" "Someday" entry (file+olp ,(concat org-file-path "someday.org") "Catchall")
         "* SOMEDAY %? \n%U" :empty-lines 1)
        ("e" "Event" entry (file ,(concat org-file-path "personal.org"))
         "* %? %^T\n%U" :empty-lines 1)
        ("j" "Journal entry" entry (function jparcill/org-journal-find-location)
         "* NEXT %?\n%U" :empty-lines 1)
        ("k" "Journal sched entry" entry (function jparcill/org-journal-find-location)
         "* %? %^T\n%U" :empty-lines 1)
        ("m" "Morning Journal entry" entry (function jparcill/org-journal-find-location)
         "* Morning Entry
** Checklist
   - [ ] Make bed meditatively and 5 min clean
   - [ ] Put on this week's album
    [[file:../../20200904120153-album_of_the_week.org][Album of the Week]]
   - [ ] Workout related Stretch
   - [ ] Ergonomic stretch
   - [ ] Bruxism stretch
   - [ ] Clean out phone.org
    [[file:../../phone.org][phone.org   ]]
   - [ ] Restructure TODOs
   - [ ] journal and day plan on other side
     - plan tips:
       - don't put mentally straining todos after working out or eating.
       - mentally chill stuff includes:
         - relaxing reading
         - article reading
         - art
   - [ ] Review whole month
   - [ ] Yesterday's journal if not done
   - [ ] Upload any images to journal
   - [ ] Refresh phone orgzly
** Looking Forward To %?
** Day Plan
** Determinations :determ:" :empty-lines 1)
        ("n" "Night Journal entry" entry (function jparcill/org-journal-find-location)
         "* My Day\n%U")
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
(setq org-refile-targets '((nil :maxlevel . 9)
		           (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
(setq org-refile-allow-creating-parent-nodes (quote confirm))
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


(load!
 "~/.doom.d/custom_packages/org-krita-0.1.1/org-krita.el"
 )
