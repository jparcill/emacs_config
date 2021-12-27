;;; org-roam-lite.el -*- lexical-binding: t; -*-

(defun jparcill/org-roam-lite-new-note ()
  (interactive)
  (let ((note-name (read-string "Name of note:")))
    (find-file (concat org-file-path  (format-time-string "%Y%m%d%H%M%S" (current-time)) "-" note-name ".org"))
  )
)

(defun jparcill/org-roam-lite-search-org ()
  (interactive)
  (let ((search-text (read-string "Search: ")))
     (rgrep search-text "*.org" org-file-path nil)))
