;;; org-roam-lite.el -*- lexical-binding: t; -*-

;; https://stackoverflow.com/questions/66574715/how-to-get-org-mode-file-title-and-other-file-level-properties-from-an-arbitra
(defun ndk/get-keyword-key-value (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key)
          (plist-get data :value))))

(defun ndk/org-current-buffer-get-title ()
  (nth 1
       (assoc "TITLE"
              (org-element-map (org-element-parse-buffer 'greater-element)
                  '(keyword)
                #'ndk/get-keyword-key-value))))

(defun ndk/org-file-get-title (file)
  (with-current-buffer (find-file-noselect file)
    (ndk/org-current-buffer-get-title)))


(defun jparcill/org-roam-lite-new-note ()
  (interactive)
  (let ((note-name (read-string "Name of note:")))
    (find-file (concat org-file-path  (format-time-string "%Y%m%d%H%M%S" (current-time)) "-" note-name ".org"))
    )
  )

(defun jparcill/org-roam-lite-search-org ()
  (interactive)
  (let ((search-text (read-string "Search: ")))
    (grep (concat "grep --color=auto -r -niH  --include=\*.org --null \"" search-text  "\" " org-file-path))))

(defun jparcill/org-roam-lite-link-note (file-name)
  (interactive
        (list (read-file-name "File Name: " org-file-path)))
  (progn (message (concat "inserted " file-name))
    (insert "[[file:" file-name "][" (ndk/org-file-get-title file-name) "]]")))
