;;; mjolnir-echo.el --- Echo area display for Mjolnir -*- lexical-binding: t -*-

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

;; Echo area and overlay display implementation for Mjolnir.
;; Handles both echo messages and window preview overlays.

;;; Code:

(require 'mjolnir-state)

;;; Custom Variables

(defcustom mjolnir-echo-format-current "[%s]"
  "Format string for current buffer in echo area."
  :type 'string
  :group 'mjolnir)

(defcustom mjolnir-echo-format-other "%s"
  "Format string for other buffers in echo area."
  :type 'string
  :group 'mjolnir)

(defcustom mjolnir-echo-max-length 20
  "Maximum length of buffer names in echo area."
  :type 'integer
  :group 'mjolnir)

(defcustom mjolnir-echo-timeout 2.0
  "Seconds to show echo message before clearing."
  :type 'number
  :group 'mjolnir)

(defcustom mjolnir-preview-duration 2.0
  "Seconds to show preview overlays."
  :type 'number
  :group 'mjolnir)

(defcustom mjolnir-display-method 'echo
  "Display method for Mjolnir feedback.
\\='echo - show in echo area
\\='overlays - show window numbers
\\='both - show both"
  :type '(choice (const :tag "Echo area" echo)
                 (const :tag "Overlays" overlays) 
                 (const :tag "Both" both))
  :group 'mjolnir)

;;; Faces

(defface mjolnir-preview-number
  '((t :weight bold
       :height 2.0
       :inherit highlight))
  "Face for preview overlay numbers.
Inherits from `highlight' face to respect theme colors.
Theme authors should customize `highlight' to affect this face."
  :group 'mjolnir)

;;; Internal Variables

(defvar mjolnir--echo-timer nil
  "Timer for clearing echo area.")

(defvar mjolnir--preview-overlays nil
  "Active preview overlays.")

(defvar mjolnir--preview-timer nil
  "Timer for clearing preview overlays.")

(defvar mjolnir--display-cleanup nil
  "Active cleanup function.")

;;; Display API

(defun mjolnir--display (info)
  "Display INFO using configured method."
  (mjolnir--clear-display)
  (pcase mjolnir-display-method
    ('echo (mjolnir--display-echo info))
    ('overlays (mjolnir--display-overlays info))
    ('both (mjolnir--display-echo info)
           (mjolnir--display-overlays info))))

(defun mjolnir--clear-display ()
  "Clear any active display."
  (when mjolnir--display-cleanup
    (ignore-errors (funcall mjolnir--display-cleanup))
    (setq mjolnir--display-cleanup nil))
  (mjolnir--clear-echo)
  (mjolnir--clear-overlays))

;;; Echo Display

(defun mjolnir--display-echo (info)
  "Display INFO in echo area."
  (let ((action (plist-get info :action)))
    (pcase action
      ('rotate (mjolnir--echo-rotation info))
      ('summon (mjolnir--echo-summoning info))
      ('preview (message "⚡ Window rotation order"))
      (_ (message "Mjolnir: %s" action))))
  (mjolnir--schedule-echo-clear))

(defun mjolnir--echo-rotation (info)
  "Display rotation feedback from INFO."
  (let ((count (plist-get info :count))
        (direction (plist-get info :direction)))
    (message "⚡ Rotated %d windows %s"
             count
             (if (eq direction 'forward) "clockwise" "counter-clockwise"))))

(defun mjolnir--echo-summoning (info)
  "Display summoning feedback from INFO."
  (let* ((buffers (plist-get info :buffers))
         (position (plist-get info :position))
         (names (mjolnir--format-buffer-list buffers position)))
    (message "%s" names)))

(defun mjolnir--format-buffer-list (buffers position)
  "Format BUFFERS list with current at POSITION."
  (let* ((window-width (window-width))
         (names (mapcar (lambda (buf)
                         (mjolnir--truncate-name (buffer-name buf)))
                       buffers))
         (visible (mjolnir--compute-visible-range 
                  names position window-width)))
    ;; Build display string
    (mapconcat (lambda (idx)
                 (let ((name (nth idx names)))
                   (if (= idx position)
                       (format mjolnir-echo-format-current name)
                     (format mjolnir-echo-format-other name))))
               visible " ")))

(defun mjolnir--truncate-name (name)
  "Truncate NAME to fit echo area."
  (if (<= (length name) mjolnir-echo-max-length)
      name
    (concat (substring name 0 (- mjolnir-echo-max-length 1)) "…")))

(defun mjolnir--compute-visible-range (names position max-width)
  "Compute range of NAMES visible around POSITION within MAX-WIDTH."
  (let ((start position)
        (end position)
        (current-width (+ 2 (length (nth position names))))
        visible)
    ;; Always include current
    (push position visible)
    ;; Expand range
    (while (and (< current-width (- max-width 5))
                (or (> start 0) (< end (1- (length names)))))
      ;; Try right
      (when (and (< end (1- (length names)))
                 (let ((width (+ 3 (length (nth (1+ end) names)))))
                   (< (+ current-width width) (- max-width 5))))
        (cl-incf end)
        (push end visible)
        (cl-incf current-width (+ 3 (length (nth end names)))))
      ;; Try left
      (when (and (> start 0)
                 (let ((width (+ 3 (length (nth (1- start) names)))))
                   (< (+ current-width width) (- max-width 5))))
        (cl-decf start)
        (push start visible)
        (cl-incf current-width (+ 3 (length (nth start names))))))
    (sort visible #'<)))

(defun mjolnir--schedule-echo-clear ()
  "Schedule clearing of echo area."
  (when mjolnir--echo-timer
    (cancel-timer mjolnir--echo-timer)
    (setq mjolnir--echo-timer nil))
  (when mjolnir-echo-timeout
    (setq mjolnir--echo-timer
          (run-with-idle-timer mjolnir-echo-timeout nil
                              #'mjolnir--clear-echo))))

(defun mjolnir--clear-echo ()
  "Clear echo area message."
  (when mjolnir--echo-timer
    (cancel-timer mjolnir--echo-timer)
    (setq mjolnir--echo-timer nil))
  (message nil))

;;; Overlay Display

(defun mjolnir--display-overlays (info)
  "Display overlays based on INFO."
  (let ((action (plist-get info :action))
        (windows (plist-get info :windows)))
    (when (and windows (memq action '(rotate preview)))
      (mjolnir--show-window-numbers windows))))

(defun mjolnir--show-window-numbers (windows)
  "Show overlay numbers on WINDOWS."
  (mjolnir--clear-overlays)
  (let ((counter 1))
    (dolist (window windows)
      (when (window-live-p window)
        (let ((overlay (make-overlay 1 1 (window-buffer window))))
          (overlay-put overlay 'window window)
          (overlay-put overlay 'priority 1000)
          (overlay-put overlay 'before-string
                       (propertize (format " %d " counter)
                                  'face 'mjolnir-preview-number))
          (push overlay mjolnir--preview-overlays)
          (cl-incf counter)))))
  (when mjolnir-preview-duration
    (setq mjolnir--preview-timer
          (run-at-time mjolnir-preview-duration nil
                      #'mjolnir--clear-overlays))))

(defun mjolnir--clear-overlays ()
  "Clear all preview overlays."
  (when mjolnir--preview-timer
    (cancel-timer mjolnir--preview-timer)
    (setq mjolnir--preview-timer nil))
  (mapc #'delete-overlay mjolnir--preview-overlays)
  (setq mjolnir--preview-overlays nil))

(provide 'mjolnir-echo)
;;; mjolnir-echo.el ends here
