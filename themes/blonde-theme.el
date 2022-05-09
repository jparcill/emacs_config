;;; themes/blonde-theme.el -*- lexical-binding: t; -*-


(require 'doom-themes)
;;
;;TODO Make sure the 256 and 16 are correct
;;Yellow needs to be changed
;;magenta highlighting in ivy looks completely ugly
;; violet needs to be changed for tables
(defgroup blonde-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom blonde-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'blonde-theme
  :type 'boolean)

(defcustom blonde-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'blonde-theme
  :type 'boolean)

(defcustom blonde-comment-bg blonde-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'blonde-theme
  :type 'boolean)

(defcustom blonde-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'blonde-theme
  :type '(choice integer boolean))

;;
(def-doom-theme blonde
  "A theme inspired by Blonde"

  ;; name        default   256       16
  ((bg         '("#f9f9f9" nil       nil           ))
   (bg-alt     '("#f9f9f9" nil       nil           ))
   (base0      '("#e0e0e0" "black"   "black"       ))
   (base1      '("#cccccc" "#1e1e1e" "brightblack" ))
   (base2      '("#adadad" "#2e2e2e" "brightblack" ))
   (base3      '("#999999" "#262626" "brightblack" ))
   (base4      '("#8f8f8f" "#3f3f3f" "brightblack" ))
   (base5      '("#7a7a7a" "#525252" "brightblack" ))
   (base6      '("#7b7070" "#6b6b6b" "brightblack" ))
   (base7      '("#5c5c5c" "#979797" "brightblack" ))
   (base8      '("#474747" "#dfdfdf" "white"       ))
   (fg-alt     '("#688D5D" "#2d2d2d" "white"       ))
   (fg         '("#688D5D" "#bfbfbf" "brightwhite" ))

   (grey       '("#498caf" "#515154" "brightblack"  ))
   (red        '("#E80000" "#a1687b" "red"          ))
   (orange     '("#EE8700" "#E6C000" "brightred"    ))
   (green      '("#688d5d" "#C2FFDF" "green"        ))
   (yellow     '("#f9c80e" "#f9e10a" "yellow"       ))
   (blue       '("#0000ED" "#bee3e1" "brightblue"   ))
   (teal       '("#47bed2" "#47bed2" "brightgreen"  ))
   (dark-blue  '("#1244c6" "#1244c6" "blue"         ))
   (magenta    '("#EB00EC" "#c45385" "magenta"      ))
   (violet     '("#DEC0F1" "#C5A3FF" "brightmagenta"))
   (cyan       '("#73e8dc" "#C2FFDF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#204052" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      green)
   (vertical-bar   base0)
   (selection      teal)
   (builtin        blue)
   (comments       (if blonde-brighter-comments base8 base6))
   (doc-comments   base7)
   (constants      magenta)
   (functions      red)
   (keywords       magenta)
   (methods        orange)
   (operators      magenta)
   (type           blue)
   (strings        orange)
   (variables      red)
   (numbers        magenta)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    magenta)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright blonde-brighter-modeline)
   (-modeline-pad
    (when blonde-padded-modeline
      (if (integerp blonde-padded-modeline) blonde-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base8)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base2))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base2))))
   (modeline-bg-inactive   (doom-darken bg 0.01))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  (
   (company-tooltip-selection     :background base3)
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground fg)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if blonde-comment-bg base4))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (font-lock-keyword-face
    :slant 'italic
    :foreground keywords)

   ;; Centaur tabs
   (centaur-tabs-active-bar-face :background blue)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected
					  :foreground blue)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected
					    :foreground blue)
   ;; Doom modeline
   (doom-modeline-bar :background blue)

   ;; highlight-thing highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)

   ;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)

   ;; hl-line
   (hl-line :background base0)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright cyan highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (tooltip              :background bg-alt :foreground fg)
   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground magenta)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; magit
   (magit-diff-removed                :foreground (doom-darken red 0.2) :background (doom-blend red base5 0.1))
   (magit-diff-removed-highlight      :foreground red                   :background (doom-blend red base5 0.2) :weight 'bold)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground base8)
   ((outline-2 &override) :foreground base7)
   ((outline-3 &override) :foreground base5)
   ((outline-4 &override) :foreground base4)
   ((outline-5 &override) :foreground base8)
   ((outline-6 &override) :foreground base7)
   ((outline-7 &override) :foreground base5)
   ((outline-8 &override) :foreground base4)

   ;; org-mode
   ((org-block &override) :background bg)
   ((org-block-begin-line &override) :background bg)
   (org-scheduled         :foreground green)
   (org-scheduled-previously :foreground red)
   (org-scheduled-today   :foreground orange)
   (org-hide              :foreground hidden))

  ;; --- extra variables ---------------------
  ;; ()
  )

;;; blonde-theme.el ends here
