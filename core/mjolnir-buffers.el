;;; mjolnir-buffers.el --- Buffer summoning for Mjolnir -*- lexical-binding: t -*-

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

;; Buffer summoning implementation - the hidden realm of Mjolnir.

;;; Code:

(require 'mjolnir-core)
(require 'mjolnir-echo)

;;; Custom Variables

(defcustom mjolnir-reset-timeout 2.0
  "Seconds before resetting summoning position.
Set to nil to disable automatic reset."
  :type '(choice (const :tag "Never reset" nil)
                 number)
  :group 'mjolnir)

;;; Public Commands

;;;###autoload
(defun mjolnir-summon-next (&optional arg)
  "Summon next hidden buffer to current window.
With prefix ARG, summon ARG buffers forward."
  (interactive "p")
  (mjolnir--summon-buffer (or arg 1)))

;;;###autoload
(defun mjolnir-summon-previous (&optional arg)
  "Summon previous hidden buffer to current window.
With prefix ARG, summon ARG buffers backward."
  (interactive "p")
  (mjolnir--summon-buffer (- (or arg 1))))

;;; Internal Implementation

(defun mjolnir--summon-buffer (steps)
  "Summon buffer STEPS positions away."
  (let ((state (mjolnir--ensure-summon-state)))
    (when state
      (let* ((buffers (mjolnir--state-buffer-lru state))
             (count (length buffers))
             (old-pos (mjolnir--state-buffer-position state))
             (new-pos (mod (+ old-pos steps) count))
             (buffer (nth new-pos buffers)))
        ;; Update state
        (setf (mjolnir--state-buffer-position state) new-pos
              (mjolnir--state-last-command-time state) (current-time))
        ;; Switch to buffer
        (switch-to-buffer buffer t)
        ;; Display feedback
        (mjolnir--display
         (list :action 'summon
               :buffers buffers
               :position new-pos
               :count count))))))

(defun mjolnir--ensure-summon-state ()
  "Ensure summoning state is initialized and valid."
  (let ((state (mjolnir--get-state)))
    (when (or (not (mjolnir--state-buffer-lru state))
              (not (eq last-command 'mjolnir--summon))
              (mjolnir--summon-timeout-p state))
      ;; Rebuild buffer list
      (let ((candidates (mjolnir--summonable-buffers)))
        (if (null candidates)
            (progn
              (message "No buffers to summon")
              (setq state nil))
          (setf (mjolnir--state-buffer-lru state) candidates
                (mjolnir--state-buffer-position state) 0
                (mjolnir--state-original-buffer state) (current-buffer)))))
    (setq this-command 'mjolnir--summon)
    state))

(defun mjolnir--summon-timeout-p (state)
  "Check if summoning state has timed out."
  (and mjolnir-reset-timeout
       (mjolnir--state-last-command-time state)
       (> (float-time (time-since (mjolnir--state-last-command-time state)))
          mjolnir-reset-timeout)))

(provide 'mjolnir-buffers)
;;; mjolnir-buffers.el ends here
