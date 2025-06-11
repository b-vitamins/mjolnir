;;; mjolnir.el --- Whosoever holds this hammer, if he be worthy, shall possess the power of Thor. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Maintainer: Ayan Das <bvits@riseup.net>
;; Version: 0.4.0-rc1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: buffers, windows, convenience
;; URL: https://github.com/b-vitamins/mjolnir

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
;;
;; `mjolnir-mode` is an Emacs minor mode that bestows upon you the might of Thor!
;;
;; When wielding Mjölnir, nothing shall come in the way of your buffers as they thunder through your windows.  Instead of moving over to the window holding the buffer worthy of your attention,  summon it into the window you're already in.  However, not all buffers you deem worthy - let be them smitten under the might of Mjölnir - and they shall stay their ground.
;;
;; *May your windows be ever steadfast and your code mighty!* ⚡
;;
;; Works with Emacs 25.1 and later. Full power unleashed with Emacs 29+!
;;; Installation:
;; Clone this repository and add it to your `load-path`.
;;
;; (add-to-list 'load-path "/path/to/mjolnir")
;; (require 'mjolnir)
;;; Usage:
;; Enable `mjolnir-mode`:
;; (mjolnir-mode t)
;;
;; Now wield the power of Mjölnir wherever you are (`*`).
;;
;; ### Visible Realm
;;
;; Go this way (`M-n`):
;;
;; +----+----+    +----+----+    +----+----+    +----+----+
;; | A* | B  |    | B* | C  |    | C* | D  |    | D* | A  |
;; +----+----+ -> +----+----+ -> +----+----+ -> +----+----+
;; | D  | C  |    | A  | D  |    | B  | A  |    | C  | B  |
;; +----+----+    +----+----+    +----+----+    +----+----+
;;
;; Or that way (`M-p`):
;;
;; +----+----+    +----+----+    +----+----+    +----+----+
;; | A* | B  |    | D* | A  |    | C* | D  |    | B* | C  |
;; +----+----+ -> +----+----+ -> +----+----+ -> +----+----+
;; | D  | C  |    | C  | B  |    | B  | A  |    | A  | D  |
;; +----+----+    +----+----+    +----+----+    +----+----+
;;
;; Make `X` unworthy (`M-u`), then go this way (`M-n`):
;;
;; +----+----+    +----+----+    +----+----+    +----+----+
;; | A* | B  |    | B* | C  |    | C* | A  |    | A* | B  |
;; +----+----+ -> +----+----+ -> +----+----+ -> +----+----+
;; | X  | C  |    | X  | A  |    | X  | B  |    | X  | C  |
;; +----+----+    +----+----+    +----+----+    +----+----+
;;
;; ### Hidden Realm
;;
;; Summon this way (`M-N`):
;;
;; +----+----+    +----+----+    +----+----+    +----+----+
;; | A* | B  |    | G* | B  |    | F* | B  |    | E* | B  |
;; +----+----+ -> +----+----+ -> +----+----+ -> +----+----+
;; | D  | C  |    | D  | C  |    | D  | C  |    | D  | C  |
;; +----+----+    +----+----+    +----+----+    +----+----+
;;   E  F  G        A  E  F        G  A  E        F  G  A
;;
;; Or summon that way (`M-P`):
;;
;; +----+----+    +----+----+    +----+----+    +----+----+
;; | A* | B  |    | E* | B  |    | F* | B  |    | G* | B  |
;; +----+----+ -> +----+----+ -> +----+----+ -> +----+----+
;; | D  | C  |    | D  | C  |    | D  | C  |    | D  | C  |
;; +----+----+    +----+----+    +----+----+    +----+----+
;;   E  F  G        F  G  A        G  A  E        A  E  F
;;
;; Change your grip using `mjolnir-rotate-forward-key`, `mjolnir-rotate-backward-key`, `mjolnir-summon-next-key`, `mjolnir-summon-previous-key`, and `mjolnir-toggle-worthy-key`.

;;; Code:

(require 'cl-lib)

;; Load core modules (byte-compiled but not separately installed)
(let ((mjolnir-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "core" mjolnir-dir))
  (add-to-list 'load-path (expand-file-name "features" mjolnir-dir))
  (add-to-list 'load-path (expand-file-name "ui" mjolnir-dir)))

(require 'mjolnir-state)
(require 'mjolnir-core)
(require 'mjolnir-windows)
(require 'mjolnir-buffers)

;;; Keymap

(defvar mjolnir-mode-map (make-sparse-keymap)
  "Keymap for `mjolnir-mode'.")

(defun mjolnir--define-key (symbol value map)
  "Helper to update keybinding from SYMBOL to VALUE in MAP."
  (let ((command (intern (replace-regexp-in-string
                         "-key$" "" (symbol-name symbol)))))
    (when (and (boundp symbol)
               (symbol-value symbol)
               map)
      (define-key map (kbd (symbol-value symbol)) nil))
    (set-default-toplevel-value symbol value)
    (when (and value map (fboundp command))
      (define-key map (kbd value) command)))
  value)

;;; Custom Variables

(defgroup mjolnir nil
  "Lightning-fast buffer management with the power of Thor."
  :group 'convenience
  :prefix "mjolnir-"
  :link '(url-link :tag "GitHub" "https://github.com/b-vitamins/mjolnir"))

(defun mjolnir--update-key (symbol value)
  "Update keybinding for SYMBOL to VALUE.
Used as custom setter for keybinding variables."
  (mjolnir--define-key symbol value mjolnir-mode-map))

(defcustom mjolnir-rotate-forward-key "M-n"
  "Key binding for rotating buffers forward."
  :type '(choice key-sequence string)
  :set #'mjolnir--update-key
  :group 'mjolnir)

(defcustom mjolnir-rotate-backward-key "M-p"
  "Key binding for rotating buffers backward."
  :type '(choice key-sequence string)
  :set #'mjolnir--update-key
  :group 'mjolnir)

(defcustom mjolnir-summon-next-key "M-N"
  "Key binding for summoning next hidden buffer."
  :type '(choice key-sequence string)
  :set #'mjolnir--update-key
  :group 'mjolnir)

(defcustom mjolnir-summon-previous-key "M-P"
  "Key binding for summoning previous hidden buffer."
  :type '(choice key-sequence string)
  :set #'mjolnir--update-key
  :group 'mjolnir)

(defcustom mjolnir-toggle-worthy-key "M-u"
  "Key binding for toggling window/buffer worthiness."
  :type '(choice key-sequence string)
  :set #'mjolnir--update-key
  :group 'mjolnir)

(defcustom mjolnir-invoke-key "M-o"
  "Key binding for invoking transient interface."
  :type '(choice key-sequence string)
  :set #'mjolnir--update-key
  :group 'mjolnir)

(defcustom mjolnir-show-mode-line-indicator t
  "Whether to show worthiness indicator in mode-line."
  :type 'boolean
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (when (boundp 'mjolnir-mode)
           (force-mode-line-update t)))
  :group 'mjolnir)

(defcustom mjolnir-worthy-indicator " ⚡"
  "Mode-line indicator for worthy windows."
  :type 'string
  :group 'mjolnir)

(defcustom mjolnir-unworthy-indicator " ⚔"
  "Mode-line indicator for unworthy windows."
  :type 'string
  :group 'mjolnir)

;;; Mode Line

(defvar mjolnir--mode-line-format
  '(:eval (mjolnir--mode-line-indicator))
  "Mode line format for Mjolnir.")

(defun mjolnir--mode-line-indicator ()
  "Generate mode line indicator for current window."
  (when (and mjolnir-mode mjolnir-show-mode-line-indicator)
    (let* ((worthy (mjolnir-worthy-p (selected-window)))
           (indicator (if worthy
                         mjolnir-worthy-indicator
                       mjolnir-unworthy-indicator)))
      ;; Fallback for undisplayable characters
      (when (and (not worthy)
                 (not (char-displayable-p (aref indicator 1))))
        (setq indicator " ✖"))
      (propertize indicator
                  'face (if worthy 'success 'warning)
                  'help-echo (if worthy
                               "Window participates in rotation"
                             "Window is fixed")))))

;;; Keybinding Management

(defun mjolnir--setup-keybindings ()
  "Initialize all keybindings."
  (mjolnir--update-key 'mjolnir-rotate-forward-key mjolnir-rotate-forward-key)
  (mjolnir--update-key 'mjolnir-rotate-backward-key mjolnir-rotate-backward-key)
  (mjolnir--update-key 'mjolnir-summon-next-key mjolnir-summon-next-key)
  (mjolnir--update-key 'mjolnir-summon-previous-key mjolnir-summon-previous-key)
  (mjolnir--update-key 'mjolnir-toggle-worthy-key mjolnir-toggle-worthy-key)
  (mjolnir--update-key 'mjolnir-invoke-key mjolnir-invoke-key))

;;; Public Commands

;;;###autoload
(defun mjolnir-invoke (&optional arg)
  "Invoke Mjolnir with prefix ARG.
No prefix: show transient/help
C-u: rotate backward
C-u C-u: summon previous
Negative: rotate/summon backward N times"
  (interactive "P")
  (cond
   ((not arg)
    (if (mjolnir-has-feature-p 'transient)
        (mjolnir-transient)
      (mjolnir-describe-bindings)))
   ((equal arg '(4))
    (mjolnir-rotate-backward))
   ((equal arg '(16))
    (mjolnir-summon-previous))
   ((< (prefix-numeric-value arg) 0)
    (if (eq last-command 'mjolnir-summon-next)
        (mjolnir-summon-previous (abs (prefix-numeric-value arg)))
      (mjolnir-rotate-backward (abs (prefix-numeric-value arg)))))
   (t
    (if (eq last-command 'mjolnir-summon-next)
        (mjolnir-summon-next (prefix-numeric-value arg))
      (mjolnir-rotate-forward (prefix-numeric-value arg))))))

;;;###autoload
(defun mjolnir-toggle-worthy ()
  "Toggle current window and buffer worthiness."
  (interactive)
  (mjolnir-toggle-window-worthy)
  (mjolnir-toggle-buffer-worthy))

;;;###autoload
(defun mjolnir-describe-bindings ()
  "Describe Mjolnir key bindings."
  (interactive)
  (with-help-window (help-buffer)
    (princ "Mjolnir - Lightning-fast buffer control\n\n")
    (princ "Visible Realm (Rotation):\n")
    (princ (format "  %s - Rotate forward\n" mjolnir-rotate-forward-key))
    (princ (format "  %s - Rotate backward\n" mjolnir-rotate-backward-key))
    (princ "\nHidden Realm (Summoning):\n")
    (princ (format "  %s - Summon next\n" mjolnir-summon-next-key))
    (princ (format "  %s - Summon previous\n" mjolnir-summon-previous-key))
    (princ "\nControl:\n")
    (princ (format "  %s - Toggle worthy\n" mjolnir-toggle-worthy-key))
    (princ (format "  %s - Show menu\n" mjolnir-invoke-key))))

;;; Main Mode

;;;###autoload
(define-minor-mode mjolnir-mode
  "Lightning-fast buffer management with the power of Thor.
\\{mjolnir-mode-map}"
  :global t
  :lighter " Mjolnir"
  :keymap mjolnir-mode-map
  (if mjolnir-mode
      (progn
        ;; Setup
        (mjolnir--setup-keybindings)
        (mjolnir--state-setup)
        ;; Mode line
        (unless (member mjolnir--mode-line-format mode-line-format)
          (setq-default mode-line-format
                       (append mode-line-format (list mjolnir--mode-line-format))))
        (force-mode-line-update t)
        ;; Load optional features
        (when (mjolnir-has-feature-p 'transient)
          (require 'mjolnir-transient nil t))
        (message "⚡ Mjolnir mode enabled! The power of Thor is yours!"))
    ;; Teardown
    (mjolnir--state-teardown)
    (mjolnir--clear-display)
    ;; Mode line
    (setq-default mode-line-format
                 (delete mjolnir--mode-line-format mode-line-format))
    (force-mode-line-update t)
    (message "Mjolnir mode disabled. The hammer rests.")))

;; Feature detection moved here from mjolnir-display.el
(defconst mjolnir-features
  '((posframe . (featurep 'posframe))
    (child-frame . (>= emacs-major-version 26))
    (transient . (featurep 'transient))
    (project . (fboundp 'project-current)))
  "Available features for conditional functionality.")

(defun mjolnir-has-feature-p (feature)
  "Check if FEATURE is available."
  (eval (alist-get feature mjolnir-features)))

(provide 'mjolnir)
;;; mjolnir.el ends here
