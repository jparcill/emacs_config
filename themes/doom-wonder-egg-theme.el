;;; doom-wonder-egg-theme.el -*- no-byte-compile: t; -*-
(require 'doom-themes)
;;
;;TODO Make sure the 256 and 16 are correct
;;
(defgroup doom-wonder-egg-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom doom-wonder-egg-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-wonder-egg-theme
  :type 'boolean)

(defcustom doom-wonder-egg-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-wonder-egg-theme
  :type 'boolean)

(defcustom doom-wonder-egg-comment-bg doom-wonder-egg-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-wonder-egg-theme
  :type 'boolean)

(defcustom doom-wonder-egg-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-wonder-egg-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-wonder-egg
  "A theme inspired by Wonder Egg Priority"

  ;; name        default   256       16
  ((bg         '("#2f98d2" nil       nil           ))
   (bg-alt     '("#2073a1" nil       nil           ))
   (base0      '("#1e385c" "black"   "black"       ))
   (base1      '("#284b7b" "#1e1e1e" "brightblack" ))
   (base2      '("#325d9a" "#2e2e2e" "brightblack" ))
   (base3      '("#3c70b9" "#262626" "brightblack" ))
   (base4      '("#5685c8" "#3f3f3f" "brightblack" ))
   (base5      '("#749bd2" "#525252" "brightblack" ))
   (base6      '("#93b2dc" "#6b6b6b" "brightblack" ))
   (base7      '("#b2c8e6" "#979797" "brightblack" ))
   (base8      '("#d1def0" "#dfdfdf" "white"       ))
   (fg-alt     '("#B5B2Bd" "#2d2d2d" "white"       ))
   (fg         '("#F8F8F0" "#bfbfbf" "brightwhite" ))

   (grey       '("#656565" "#515154" "brightblack"  ))
   (red        '("#e38678" "#a1687b" "red"          ))
   (orange     '("#f69865" "#E6C000" "brightred"    ))
   (green      '("#cbfa8d" "#C2FFDF" "green"        ))
   (alt-green  '("#548c2f" "#C2FFDF" "green"        ))
   (dark-green '("#2c6b34" "#C2FFDF" "green"        ))
   (yellow     '("#f9e10a" "#f9e10a" "yellow"       ))
   (blue       '("#bee3e1" "#bee3e1" "brightblue"   ))
   (teal       '("#47bed2" "#47bed2" "brightgreen"  ))
   (dark-blue  '("#1244c6" "#1244c6" "blue"         ))
   (magenta    '("#f9b6c6" "#c45385" "magenta"      ))
   (violet     '("#b08ea2" "#C5A3FF" "brightmagenta"))
   (cyan       '("#96CBFE" "#C2FFDF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#204052" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken base6 0.5))
   (selection      teal)
   (builtin        blue)
   (comments       (if doom-wonder-egg-brighter-comments dark-blue blue))
   (doc-comments   cyan)
   (constants      cyan)
   (functions      yellow)
   (keywords       green)
   (methods        yellow)
   (operators      orange)
   (type           green)
   (strings        yellow)
   (variables      magenta)
   (numbers        cyan)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-wonder-egg-brighter-modeline)
   (-modeline-pad
    (when doom-wonder-egg-padded-modeline
      (if (integerp doom-wonder-egg-padded-modeline) doom-wonder-egg-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

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
    :background (if doom-wonder-egg-comment-bg (doom-lighten bg 0.05)))
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
   ;;(hl-line :background blue)
   (hl-line :background base3)

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
   (css-proprietary-property :foreground orange)
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
   ((outline-1 &override) :foreground magenta)
   ((outline-2 &override) :foreground base8)
   ((outline-3 &override) :foreground blue)
   ((outline-4 &override) :foreground magenta)
   ((outline-5 &override) :foreground base8)
   ((outline-6 &override) :foreground blue)
   ((outline-7 &override) :foreground magenta)
   ((outline-8 &override) :foreground base7)

   ;; org-mode
   ((org-block &override) :background base4)
   ((org-block-begin-line &override) :background base4)
   (org-scheduled         :foreground green)
   (org-scheduled-previously :foreground red)
   (org-scheduled-today   :foreground orange)
   (org-hide              :foreground hidden))

  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-wonder-egg-theme.el ends here
