;;; mjolnir-core.el --- Core algorithms and filters for Mjolnir -*- lexical-binding: t -*-

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

;; Core algorithms for window angle calculation, buffer ordering,
;; and worthiness filtering.

;;; Code:

(require 'cl-lib)
(require 'mjolnir-state)

;;; Custom Variables - Filters

(defcustom mjolnir-unworthy-modes
  '(treemacs-mode
    help-mode
    eldoc-mode
    compilation-mode
    grep-mode
    occur-mode
    messages-buffer-mode
    inferior-python-mode
    inferior-ess-mode
    comint-mode
    special-mode
    dired-sidebar-mode)
  "List of major modes whose windows are unworthy of rotation."
  :type '(repeat symbol)
  :group 'mjolnir)

(defcustom mjolnir-boring-buffer-regexp
  "\\` \\|\\`\\*Messages\\*\\'"
  "Regexp matching buffer names unworthy of summoning."
  :type 'regexp
  :group 'mjolnir)

(defcustom mjolnir-minimum-window-height 4
  "Windows shorter than this are automatically unworthy."
  :type 'integer
  :group 'mjolnir)

(defcustom mjolnir-worthy-predicates nil
  "Hook of predicates to determine worthiness.
Each function receives a window or buffer and returns non-nil
if the object should be considered unworthy."
  :type 'hook
  :group 'mjolnir)

(defcustom mjolnir-summon-include-visible nil
  "Whether visible buffers can be summoned.
nil - only summon hidden buffers
t - include all buffers  
\\='other - exclude buffers visible on current frame"
  :type '(choice (const :tag "Hidden only" nil)
                 (const :tag "All buffers" t)
                 (const :tag "Other frames" other))
  :group 'mjolnir)

(defcustom mjolnir-summon-project-only nil
  "When non-nil, only summon buffers from current project."
  :type 'boolean
  :group 'mjolnir)

;;; Internal Variables

(defvar mjolnir--fixed-windows nil
  "List of windows manually marked as unworthy.")

(defvar-local mjolnir-buffer-worthy t
  "Whether this buffer is worthy of summoning.")

;;; Window Angle Calculation

(defun mjolnir--frame-center (&optional frame)
  "Get center coordinates of FRAME."
  (let* ((frame (or frame (selected-frame)))
         (state (mjolnir--get-state frame))
         (cache (mjolnir--state-frame-cache state)))
    (if (and cache
             (eq (mjolnir--frame-center-frame cache) frame))
        cache
      (let ((center (make-mjolnir--frame-center
                     :frame frame
                     :x (/ (float (frame-pixel-width frame)) 2)
                     :y (/ (float (frame-pixel-height frame)) 2))))
        (setf (mjolnir--state-frame-cache state) center)
        center))))

(defun mjolnir--window-angle (window)
  "Calculate angle from frame center to WINDOW center.
Returns angle in radians."
  (let* ((state (mjolnir--get-state (window-frame window)))
         (angles (mjolnir--state-window-angles state))
         (cached (gethash window angles)))
    (or cached
        (let* ((center (mjolnir--frame-center (window-frame window)))
               (edges (window-edges window nil nil t))
               (wx (/ (float (+ (nth 0 edges) (nth 2 edges))) 2))
               (wy (/ (float (+ (nth 1 edges) (nth 3 edges))) 2))
               (dx (- wx (mjolnir--frame-center-x center)))
               (dy (- (mjolnir--frame-center-y center) wy))
               (angle (if (and (< (abs dx) 1.0) (< (abs dy) 1.0))
                         ;; Window at center - use small radian offset based on ID
                         (/ (float (mod (sxhash window) 100)) 1000.0)
                       (atan dy dx))))
          (puthash window angle angles)
          angle))))

(defun mjolnir--sort-windows-by-angle (&optional frame)
  "Get windows sorted by angle for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (state (mjolnir--get-state frame)))
    (or (mjolnir--state-sorted-windows state)
        (setf (mjolnir--state-sorted-windows state)
              (sort (window-list frame 'nomini)
                    (lambda (a b)
                      (let ((angle-a (mjolnir--window-angle a))
                            (angle-b (mjolnir--window-angle b)))
                        (if (< (abs (- angle-a angle-b)) 0.001)
                            (< (sxhash a) (sxhash b))
                          (< angle-a angle-b)))))))))

;;; Worthiness API

(defun mjolnir-worthy-p (object)
  "Return non-nil if OBJECT is worthy of Mjolnir.
OBJECT can be a window or buffer."
  (cond
   ((windowp object)
    (mjolnir--window-worthy-p object))
   ((bufferp object) 
    (mjolnir--buffer-worthy-p object))
   (t (error "Invalid object type: %S" object))))

(defun mjolnir--window-worthy-p (window)
  "Return non-nil if WINDOW is worthy of rotation."
  (and (not (memq window mjolnir--fixed-windows))
       (>= (window-height window) mjolnir-minimum-window-height)
       (not (with-current-buffer (window-buffer window)
              (memq major-mode mjolnir-unworthy-modes)))
       (not (run-hook-with-args-until-success 
             'mjolnir-worthy-predicates window))))

(defun mjolnir--buffer-worthy-p (buffer)
  "Return non-nil if BUFFER is worthy of summoning."
  (and (buffer-local-value 'mjolnir-buffer-worthy buffer)
       (not (string-match-p mjolnir-boring-buffer-regexp
                           (buffer-name buffer)))
       (not (run-hook-with-args-until-success
             'mjolnir-worthy-predicates buffer))))

;;; Buffer Ordering

(defun mjolnir--summonable-buffers ()
  "Get list of buffers available for summoning."
  (let ((visible (mjolnir--get-visible-buffers))
        (all-visible (when (eq mjolnir-summon-include-visible 'other)
                      (mjolnir--all-visible-buffers)))
        candidates)
    (dolist (buffer (buffer-list))
      (when (and (mjolnir--buffer-worthy-p buffer)
                 (or mjolnir-summon-include-visible
                     (not (memq buffer visible)))
                 (or (not (eq mjolnir-summon-include-visible 'other))
                     (not (memq buffer all-visible)))
                 (or (not mjolnir-summon-project-only)
                     (mjolnir--buffer-in-project-p buffer)))
        (push buffer candidates)))
    (nreverse candidates)))

(defun mjolnir--all-visible-buffers ()
  "Get all visible buffers across all frames."
  (let (buffers)
    (dolist (frame (frame-list))
      (dolist (window (window-list frame 'nomini))
        (push (window-buffer window) buffers)))
    buffers))

(defun mjolnir--buffer-in-project-p (buffer)
  "Return non-nil if BUFFER is in current project."
  (when (fboundp 'project-current)
    (let ((project-root (and (project-current)
                            (project-root (project-current)))))
      (when project-root
        (string-prefix-p project-root
                        (buffer-local-value 'default-directory buffer))))))

;;; User Commands

;;;###autoload
(defun mjolnir-toggle-window-worthy ()
  "Toggle current window's worthiness."
  (interactive)
  (let ((window (selected-window)))
    (if (memq window mjolnir--fixed-windows)
        (progn
          (setq mjolnir--fixed-windows 
                (delq window mjolnir--fixed-windows))
          (set-window-dedicated-p window nil)
          (message "⚡ Window is now worthy!"))
      (push window mjolnir--fixed-windows)
      (set-window-dedicated-p window t)
      (message "⚔ Window is now unworthy."))
    (mjolnir--invalidate-caches)))

;;;###autoload
(defun mjolnir-toggle-buffer-worthy ()
  "Toggle current buffer's worthiness."
  (interactive)
  (setq mjolnir-buffer-worthy (not mjolnir-buffer-worthy))
  (message "Buffer %s is now %s"
           (buffer-name)
           (if mjolnir-buffer-worthy "worthy ⚡" "unworthy ⚔"))
  (mjolnir--invalidate-caches))

;;;###autoload
(defun mjolnir-toggle-project-limit ()
  "Toggle project-only filtering for summoning."
  (interactive)
  (if (fboundp 'project-current)
      (progn
        (setq mjolnir-summon-project-only
              (not mjolnir-summon-project-only))
        (mjolnir--invalidate-caches)
        (message "Summoning %s to project"
                (if mjolnir-summon-project-only "limited" "not limited")))
    (user-error "Project support requires Emacs 28+")))

(provide 'mjolnir-core)
;;; mjolnir-core.el ends here
