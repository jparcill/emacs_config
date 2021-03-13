;;; exwm-config.el --- Predefined configurations  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains typical (yet minimal) configurations of EXWM.

;;; Code:
(require 'exwm)

(defun exwm-config-personal ()


  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(([?\s-r] . exwm-reset))))



  ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(([?\s-h] . [left])
          ([?\s-l] . [right])
          ([?\s-k] . [up])
          ([?\s-j] . [down])))

  ;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; Also shrink fringes to 1 pixel.
  (fringe-mode 1)
  )

;;; exwm-config.el ends here
