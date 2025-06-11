;;; mjolnir-windows.el --- Window rotation for Mjolnir -*- lexical-binding: t -*-

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

;; Window rotation implementation - the visible realm of Mjolnir.

;;; Code:

(require 'mjolnir-core)
(require 'mjolnir-echo)
(require 'seq)

;;; Public Commands

;;;###autoload
(defun mjolnir-rotate-forward (&optional arg)
  "Rotate buffers forward (clockwise) through visible windows.
With prefix ARG, rotate that many positions."
  (interactive "p")
  (mjolnir--rotate-windows (or arg 1)))

;;;###autoload
(defun mjolnir-rotate-backward (&optional arg)
  "Rotate buffers backward (counter-clockwise) through visible windows.
With prefix ARG, rotate that many positions."
  (interactive "p")
  (mjolnir--rotate-windows (- (or arg 1))))

;;;###autoload
(defun mjolnir-preview-rotation ()
  "Preview the window rotation order."
  (interactive)
  (let* ((windows (mjolnir--sort-windows-by-angle))
         (worthy (seq-filter #'mjolnir-worthy-p windows)))
    (mjolnir--display
     (list :action 'preview
           :windows worthy))))

;;; Internal Implementation

(defun mjolnir--rotate-windows (steps)
  "Rotate buffers by STEPS positions."
  (let* ((all-windows (mjolnir--sort-windows-by-angle))
         (worthy-windows (seq-filter #'mjolnir-worthy-p all-windows))
         (count (length worthy-windows)))
    (cond
     ((= count 0)
      (user-error "No worthy windows to rotate"))
     ((= count 1)
      (message "Only one worthy window - nothing to rotate"))
     (t
      ;; Save current state
      (let ((state (mjolnir--get-state)))
        (setf (mjolnir--state-rotation-snapshot state)
              (mapcar (lambda (w) (cons w (window-buffer w)))
                     worthy-windows)))
      ;; Perform rotation
      (mjolnir--perform-rotation worthy-windows steps)
      ;; Display feedback
      (mjolnir--display
       (list :action 'rotate
             :windows worthy-windows
             :count count
             :direction (if (> steps 0) 'forward 'backward)))))))

(defun mjolnir--perform-rotation (windows steps)
  "Rotate buffers through WINDOWS by STEPS positions."
  (let* ((count (length windows))
         (offset (mod steps count))
         (snapshot (mapcar (lambda (w) (cons w (window-buffer w)))
                          windows)))
    (condition-case err
        (dotimes (i count)
          (let* ((src-idx i)
                 (dst-idx (mod (+ i offset) count))
                 (window (car (nth src-idx snapshot)))
                 (buffer (cdr (nth dst-idx snapshot))))
            (when (window-live-p window)
              (set-window-buffer window buffer))))
      (error
       (message "⚠ Error during rotation: %s" err)
       ;; Try to restore original state
       (dolist (pair snapshot)
         (when (window-live-p (car pair))
           (ignore-errors
             (set-window-buffer (car pair) (cdr pair)))))))))

(provide 'mjolnir-windows)
;;; mjolnir-windows.el ends here
