;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-super-agenda)
(package! browse-kill-ring)
(package! exwm)
(package! org-download)
(package! ox-hugo)
(package! polymode)
(package! zmq)
(package! shrface)
(package! exwm-firefox :recipe
  (:host github :repo "ieure/exwm-firefox"))
(package! base16-theme)
(package! olivetti)
(package! counsel-spotify)
(package! winner :disable t)
(package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))
(package! exwm-edit)
(package! nov)
(package! mathpix.el :recipe (:host github :repo "jethrokuan/mathpix.el"))