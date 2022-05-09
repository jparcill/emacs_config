;;; .doom.d/config.el -*- lexical-biding: t; -*-

;; DATE
;;[2019-11-26 Tue]
;; My own config functions

;;
;; Device Variables
;; ~~~~~~~~~~~~~~~~



;; Private
;; --------
(load! "private.el")

;; Machine dependent-code
;; ----------------------

;; Linux Specific
(cond ((equal (string-trim (shell-command-to-string "hostname"))  "jared-virtualbox")
       (load! "./machine_specific/linux_pc.el"))
      ((equal (string-trim (shell-command-to-string "hostname")) "localhost")
       (load! "./machine_specific/phone.el"))
      ((equal (string-trim (shell-command-to-string "hostname")) "DESKTOP-6DMJC51")
       (load! "./machine_specific/windows_pc.el"))
  )


;; Aesthetics
;; -----------
;; Linum
(setq display-line-numbers-type 'relative)

;;theme
(setq doom-theme 'blonde)

;; Taken from https://tecosaur.github.io/emacs-config/config.html
(defun jparcill/doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'jparcill/doom-modeline-conditional-buffer-encoding)


;; Package Specific Code In Order Of Importance
;; ------------------------------------------------------
(load! "core-func.el")
(load! "secondary-func.el")
;;(load! "extra-func.el")

;; Custom Functions
;; ----------------
;;

(load! "org-roam-lite.el")

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


;; Function to export a temporary pdf
(defun jparcill/tmp-org-to-pdf ()
  (interactive)
  (let ((new-file-name (concat "/tmp/" (buffer-name) ".pdf")))
    (start-process-shell-command
     (concat (buffer-name) "-tmp-pdf")
     (concat (buffer-name) "-tmp-pdf")
     (concat "pandoc -o " new-file-name " " (buffer-file-name)))
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
     (concat "\n* Images\n" (jparcill/img-path-string date))
     nil
     (concat org-journal-dir (concat date ".org"))
     )
    )
  )

(defun jparcill/go-to-org ()
  "Go to org directory"
  (interactive)
  (find-file org-file-path)
)


(defun jparcill/open-with-xournal ()
  (interactive)
  (start-process-shell-command (f-this-file) (f-this-file) (concat "xournalpp " (f-this-file))))

;; Keybindings
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(global-set-key (kbd "C-c b") 'browse-kill-ring)
(global-set-key (kbd "C-c c") '=calendar)
(global-set-key (kbd "C-c l m") 'mathpix-screenshot)
(global-set-key (kbd "C-c o o") 'jparcill/go-to-org)
(global-set-key (kbd "C-c o n") 'jparcill/org-roam-lite-new-note)
(global-set-key (kbd "C-c o s") 'jparcill/org-roam-lite-search-org)

(if (not (equal (string-trim (shell-command-to-string "hostname"))  "jared-virtualbox"))
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

(setq gc-cons-threshold (* 2 1000 1000)) ;; https://blog.d46.us/advanced-emacs-startup/
