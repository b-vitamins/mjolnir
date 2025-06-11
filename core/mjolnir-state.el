;;; mjolnir-state.el --- State management for Mjolnir -*- lexical-binding: t -*-

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

;; State management and caching for Mjolnir operations.
;; Handles frame-local state and cache invalidation.

;;; Code:

(require 'cl-lib)

;;; State Structures

(cl-defstruct mjolnir--state
  "Unified state for Mjolnir operations."
  ;; Shared
  frame-cache
  worthy-windows
  last-command-time
  ;; Hidden realm (summoning)
  buffer-lru
  buffer-position
  original-buffer
  ;; Visible realm (rotation)
  window-angles
  rotation-snapshot
  sorted-windows)

(cl-defstruct mjolnir--frame-center
  "Cached frame center coordinates."
  frame x y)

;;; State Management

(defvar mjolnir--global-state nil
  "Global state for frame-independent operations.")

(defun mjolnir--get-state (&optional frame)
  "Get or create state for FRAME (default: current frame)."
  (let ((frame (or frame (selected-frame))))
    (or (frame-parameter frame 'mjolnir--state)
        (let ((state (make-mjolnir--state
                      :last-command-time (current-time)
                      :window-angles (make-hash-table :test 'eq))))
          (set-frame-parameter frame 'mjolnir--state state)
          state))))

(defun mjolnir--clear-state (&optional frame)
  "Clear state for FRAME (default: current frame)."
  (let ((frame (or frame (selected-frame))))
    (set-frame-parameter frame 'mjolnir--state nil)))

;;; Cache Management

(defvar mjolnir--visible-buffers-cache nil
  "Cache of (FRAME . (TICK . BUFFERS)) for visible buffer tracking.")

(defvar mjolnir--buffer-list-tick nil
  "Last buffer-list-update-tick value.")

;; Compatibility for older Emacs versions
(unless (fboundp 'buffer-list-update-tick)
  (defvar mjolnir--emulated-tick 0)
  (defun buffer-list-update-tick ()
    mjolnir--emulated-tick)
  (add-hook 'buffer-list-update-hook
            (lambda () (cl-incf mjolnir--emulated-tick))))

(defun mjolnir--cache-valid-p ()
  "Check if caches are still valid."
  (and mjolnir--buffer-list-tick
       (= mjolnir--buffer-list-tick (buffer-list-update-tick))))

(defun mjolnir--invalidate-caches ()
  "Invalidate all caches."
  (setq mjolnir--visible-buffers-cache nil
        mjolnir--buffer-list-tick nil)
  (dolist (frame (frame-list))
    (let ((state (frame-parameter frame 'mjolnir--state)))
      (when state
        (clrhash (mjolnir--state-window-angles state))
        (setf (mjolnir--state-sorted-windows state) nil
              (mjolnir--state-frame-cache state) nil
              (mjolnir--state-worthy-windows state) nil)))))

(defun mjolnir--update-tick ()
  "Update buffer list tick."
  (setq mjolnir--buffer-list-tick (buffer-list-update-tick)))

(defun mjolnir--get-visible-buffers (&optional frame)
  "Get visible buffers for FRAME with proper frame-local caching."
  (let* ((frame (or frame (selected-frame)))
         (tick (buffer-list-update-tick))
         (cached (assq frame mjolnir--visible-buffers-cache)))
    (if (and cached
             (eq (cadr cached) tick))
        (cddr cached)
      ;; Rebuild cache for this frame
      (let (buffers)
        (dolist (window (window-list frame 'nomini))
          (push (window-buffer window) buffers))
        ;; Update cache
        (if cached
            (setcdr cached (cons tick buffers))
          (push (cons frame (cons tick buffers))
                mjolnir--visible-buffers-cache))
        buffers))))

;; Clean up cache when frames are deleted
(add-hook 'delete-frame-functions
          (lambda (frame)
            (setq mjolnir--visible-buffers-cache
                  (assq-delete-all frame mjolnir--visible-buffers-cache))))

;;; Hooks

(defun mjolnir--state-setup ()
  "Set up state management hooks."
  (add-hook 'window-configuration-change-hook #'mjolnir--invalidate-caches)
  (add-hook 'window-size-change-functions 
            (lambda (_) (mjolnir--invalidate-caches))))

(defun mjolnir--state-teardown ()
  "Remove state management hooks."
  (remove-hook 'window-configuration-change-hook #'mjolnir--invalidate-caches)
  (remove-hook 'window-size-change-functions 
               (lambda (_) (mjolnir--invalidate-caches)))
  (mjolnir--invalidate-caches)
  (dolist (frame (frame-list))
    (mjolnir--clear-state frame)))

(provide 'mjolnir-state)
;;; mjolnir-state.el ends here
