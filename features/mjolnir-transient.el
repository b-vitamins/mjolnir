;;; mjolnir-transient.el --- Transient interface for Mjolnir -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient UI for Mjolnir commands.
;; Only loaded when transient is available.

;;; Code:

(require 'transient nil t)

;;; Transient Definition

(when (featurep 'transient)
  
  ;;;###autoload
  (transient-define-prefix mjolnir-transient ()
    "⚡ Mjolnir - Lightning-fast buffer control"
    ["Visible Realm - Rotate"
     :class transient-row
     ("n" "→ Forward" mjolnir-rotate-forward :transient t)
     ("p" "← Backward" mjolnir-rotate-backward :transient t)
     ("v" "Preview order" mjolnir-preview-rotation)]
    ["Hidden Realm - Summon"
     :class transient-row  
     ("N" "↓ Next" mjolnir-summon-next :transient t)
     ("P" "↑ Previous" mjolnir-summon-previous :transient t)]
    ["Control"
     :class transient-row
     ("u" "Toggle window worthy" mjolnir-toggle-window-worthy)
     ("U" "Toggle buffer worthy" mjolnir-toggle-buffer-worthy)
     ("j" "Toggle project limit" mjolnir-toggle-project-limit)]
    ["Display"
     :class transient-row
     ("de" "Echo area" 
      (lambda () (interactive) 
        (setq mjolnir-display-method 'echo)
        (message "Display: echo area"))
      :transient t)
     ("do" "Overlays" 
      (lambda () (interactive)
        (setq mjolnir-display-method 'overlays)
        (message "Display: overlays"))
      :transient t)
     ("db" "Both" 
      (lambda () (interactive)
        (setq mjolnir-display-method 'both)
        (message "Display: both"))
      :transient t)]
    ["Help"
     ("?" "Describe bindings" mjolnir-describe-bindings)
     ("q" "Quit" transient-quit-one)]))

(provide 'mjolnir-transient)
;;; mjolnir-transient.el ends here
