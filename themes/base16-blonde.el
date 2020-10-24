;;
;; Custom theme
;;

(require 'base16-theme)

(defvar base16-my-auto-colors
  '(:base00 "#17090a"
    :base01 "#281f18"
    :base02 "#1b352a"
    :base03 "#854d3e"
    :base04 "#636361"
    :base05 "#9a6b63"
    :base06 "#491909"
    :base07 "#45312e"
    :base08 "#808082"
    :base09 "#989898"
    :base0A "#bb825f"
    :base0B "#d99a7b"
    :base0C "#989898"
    :base0D "#989898"
    :base0E "#989898"
    :base0F "#808082")
  "All colors for Base16 Macoy are defined here.")

;; Define the theme
(deftheme base16-blonde)

;; Add all the faces to the theme
(base16-theme-define 'base16-blonde base16-my-auto-colors)

;; Mark the theme as provided
(provide-theme 'base16-blonde)

(provide 'base16-blonde)

;;; base16-my-auto-theme.el ends here
