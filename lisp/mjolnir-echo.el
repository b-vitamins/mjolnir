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

(require 'cl-lib)
(require 'mjolnir-state)

;;; Custom Variables

(defcustom mjolnir-echo-format-current "%s"
  "Format string for the current label before rail styling."
  :type 'string
  :group 'mjolnir)

(defcustom mjolnir-echo-format-other "%s"
  "Format string for non-current labels before rail styling."
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

(defcustom mjolnir-display-method 'both
  "Display method for Mjolnir feedback.
\\='echo - show in echo area
\\='overlays - show window numbers
\\='both - show both"
  :type '(choice (const :tag "Echo area" echo)
                 (const :tag "Overlays" overlays)
                 (const :tag "Both" both))
  :group 'mjolnir)

;;; Faces

(defface mjolnir-echo-rail
  '((t :inherit shadow))
  "Base face for the echo-area rail."
  :group 'mjolnir)

(defface mjolnir-echo-current
  '((t :inherit highlight
       :weight bold))
  "Face for the current slot in the echo-area rail."
  :group 'mjolnir)

(defface mjolnir-echo-adjacent
  '((t :weight bold))
  "Face for slots adjacent to the current one."
  :group 'mjolnir)

(defface mjolnir-echo-distant
  '((t :inherit shadow))
  "Face for distant slots in the echo-area rail."
  :group 'mjolnir)

(defface mjolnir-echo-origin
  '((t :inherit font-lock-keyword-face
       :weight bold))
  "Face for origin markers in the echo-area rail."
  :group 'mjolnir)

(defface mjolnir-echo-separator
  '((t :inherit shadow))
  "Face for separators and rail caps in the echo-area rail."
  :group 'mjolnir)

(defface mjolnir-echo-meta
  '((t :inherit shadow
       :slant italic))
  "Face for subtle metadata around the echo-area rail."
  :group 'mjolnir)

(defface mjolnir-echo-overflow
  '((t :inherit shadow
       :weight bold))
  "Face for overflow and continuation hints in the echo-area rail."
  :group 'mjolnir)

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
      ('preview (mjolnir--echo-preview info))
      (_ (mjolnir--echo-message (format "Mjolnir: %s" action)))))
  (mjolnir--schedule-echo-clear))

(defun mjolnir--echo-rotation (info)
  "Display rotation feedback from INFO."
  (let ((direction (plist-get info :direction))
        (steps (plist-get info :steps))
        (windows (plist-get info :windows))
        (current-window (plist-get info :current-window))
        (origin-window (plist-get info :origin-window)))
    (mjolnir--echo-message
     (mjolnir--format-window-order
      windows current-window origin-window
      (mjolnir--echo-meta-string
       (if (eq direction 'forward) "CW" "CCW"))
      (mjolnir--echo-meta-string (format "%+d" (or steps 0)))))))

(defun mjolnir--echo-summoning (info)
  "Display summoning feedback from INFO."
  (let* ((buffers (plist-get info :buffers))
         (scope (plist-get info :scope))
         (position (plist-get info :position))
         (origin-buffer (plist-get info :origin-buffer)))
    (mjolnir--echo-message
     (mjolnir--format-buffer-list buffers scope position origin-buffer))))

(defun mjolnir--format-buffer-list (buffers scope position origin-buffer)
  "Format BUFFERS within SCOPE as a polished rail.
POSITION is highlighted and ORIGIN-BUFFER is tracked."
  (let* ((active-buffers (cl-subseq buffers 0 scope))
         (origin-index (cl-position origin-buffer active-buffers))
         (prefix (unless origin-index
                   (mjolnir--format-origin-prefix origin-buffer)))
         (suffix (when (< scope (length buffers))
                   (mjolnir--echo-overflow-string
                    (format "+%d" (- (length buffers) scope))))))
    (mjolnir--render-rail
     (mapcar (lambda (buffer)
               (buffer-name buffer))
             active-buffers)
     position
     origin-index
     prefix
     suffix)))

(defun mjolnir--truncate-name (name)
  "Truncate NAME to fit echo area."
  (if (<= (length name) mjolnir-echo-max-length)
      name
    (concat (substring name 0 (- mjolnir-echo-max-length 1)) "…")))

(defun mjolnir--echo-preview (info)
  "Display preview feedback from INFO."
  (let ((windows (plist-get info :windows))
        (current-window (plist-get info :current-window))
        (origin-window (plist-get info :origin-window)))
    (mjolnir--echo-message
     (mjolnir--format-window-order
      windows current-window origin-window
      (mjolnir--echo-meta-string "MAP")
      nil))))

(defun mjolnir--format-window-order (windows current-window origin-window
                                             &optional prefix suffix)
  "Format WINDOWS as a polished rail.
CURRENT-WINDOW is highlighted and ORIGIN-WINDOW is marked in the rail.
Optional PREFIX and SUFFIX strings are placed around the rail."
  (let ((current-index (or (cl-position current-window windows) 0))
        (origin-index (cl-position origin-window windows)))
    (mjolnir--render-rail
     (mapcar (lambda (window)
               (buffer-name (window-buffer window)))
             windows)
     current-index
     origin-index
     prefix
     suffix)))

(defun mjolnir--echo-message (text)
  "Show TEXT in the echo area without writing it to `*Messages*'."
  (let ((message-log-max nil))
    (message "%s" text)))

(defun mjolnir--echo-meta-string (text)
  "Return TEXT styled as echo-area metadata."
  (propertize text 'face 'mjolnir-echo-meta))

(defun mjolnir--echo-overflow-string (text)
  "Return TEXT styled as an overflow hint."
  (propertize text 'face 'mjolnir-echo-overflow))

(defun mjolnir--format-origin-prefix (buffer)
  "Return a styled origin prefix for BUFFER."
  (let* ((label (mjolnir--truncate-width (buffer-name buffer) 12))
         (text (mjolnir--echo-meta-string
                (format "%s %s" (mjolnir--glyph 'origin) label))))
    (add-face-text-property 0 1 'mjolnir-echo-origin t text)
    text))

(defun mjolnir--glyph (kind)
  "Return a polished glyph for KIND with a terminal-safe fallback."
  (pcase kind
    ('rail-left (if (char-displayable-p ?‹) "‹" "<"))
    ('rail-right (if (char-displayable-p ?›) "›" ">"))
    ('current-left (if (char-displayable-p ?⟨) "⟨" "["))
    ('current-right (if (char-displayable-p ?⟩) "⟩" "]"))
    ('separator (if (char-displayable-p ?┆) "┆" "|"))
    ('origin (if (char-displayable-p ?◆) "◆" "*"))
    ('ellipsis (if (char-displayable-p ?…) "…" "."))))

(defun mjolnir--truncate-width (text width)
  "Truncate TEXT to WIDTH columns without padding."
  (if (<= (string-width text) width)
      text
    (if (<= width 1)
        (truncate-string-to-width text width)
      (concat (truncate-string-to-width text (1- width))
              (mjolnir--glyph 'ellipsis)))))

(defun mjolnir--fit-string (text width)
  "Fit TEXT into WIDTH columns with right padding."
  (let* ((truncated (mjolnir--truncate-width text width))
         (padding (- width (string-width truncated))))
    (if (> padding 0)
        (concat truncated (make-string padding ? ))
      truncated)))

(defun mjolnir--rail-layout (count width prefix suffix)
  "Return a cons of (SLOT-COUNT . SLOT-WIDTH) for COUNT items in WIDTH.
PREFIX and SUFFIX are optional styled strings around the rail."
  (let ((slot-count (min count
                         (cond
                          ((>= width 96) 5)
                          ((>= width 68) 3)
                          (t 1))))
        slot-width)
    (while
        (progn
          (setq slot-width
                (mjolnir--rail-slot-width slot-count width prefix suffix))
          (and (> slot-count 1)
               (< slot-width 8)))
      (setq slot-count (if (> slot-count 3) 3 1)))
    (cons slot-count (max 4 slot-width))))

(defun mjolnir--rail-slot-width (slot-count width prefix suffix)
  "Compute slot width for SLOT-COUNT items within WIDTH.
PREFIX and SUFFIX are optional styled strings around the rail."
  (let* ((base-width (+ 4
                        (* (max 0 (1- slot-count)) 3)
                        (if prefix (+ 2 (string-width prefix)) 0)
                        (if suffix (+ 2 (string-width suffix)) 0)))
         (available (max 4 (- width base-width))))
    (min mjolnir-echo-max-length
         (max 4 (/ available (max slot-count 1))))))

(defun mjolnir--visible-ring-indices (count current-index slot-count)
  "Return SLOT-COUNT indices around CURRENT-INDEX within COUNT items."
  (let ((start (- current-index (floor (1- slot-count) 2)))
        indices)
    (dotimes (offset slot-count)
      (push (mod (+ start offset) count) indices))
    (nreverse indices)))

(defun mjolnir--ring-distance (count a b)
  "Return the shortest ring distance between A and B within COUNT items."
  (let ((delta (abs (- a b))))
    (min delta (- count delta))))

(defun mjolnir--slot-face (distance currentp)
  "Return the appropriate rail face for DISTANCE from current.
CURRENTP selects the highlighted current face."
  (cond
   (currentp 'mjolnir-echo-current)
   ((<= distance 1) 'mjolnir-echo-adjacent)
   (t 'mjolnir-echo-distant)))

(defun mjolnir--render-slot (label width currentp distance originp)
  "Render LABEL inside a fixed WIDTH rail slot.
CURRENTP highlights the slot, DISTANCE tunes emphasis, and ORIGINP marks it."
  (let* ((format-string (if currentp
                            mjolnir-echo-format-current
                          mjolnir-echo-format-other))
         (marker-width (if (> width 8) 2 1))
         (inner-width (max 1 (- width (if currentp 2 0))))
         (marker-width (min marker-width (max 0 (1- inner-width))))
         (marker (if (> marker-width 0)
                     (if originp
                         (concat (mjolnir--glyph 'origin)
                                 (if (> marker-width 1) " " ""))
                       (make-string marker-width ? ))
                   ""))
         (text-width (max 1 (- inner-width marker-width)))
         (formatted (format format-string label))
         (inner (concat marker (mjolnir--fit-string formatted text-width)))
         (slot (if currentp
                   (concat (mjolnir--glyph 'current-left)
                           (mjolnir--fit-string inner inner-width)
                           (mjolnir--glyph 'current-right))
                 (mjolnir--fit-string inner inner-width)))
         (face (mjolnir--slot-face distance currentp))
         (marker-start (if currentp 1 0)))
    (setq slot (propertize slot 'face face))
    (when originp
      (add-face-text-property marker-start
                              (min (length slot) (1+ marker-start))
                              'mjolnir-echo-origin
                              t
                              slot))
    slot))

(defun mjolnir--render-rail (labels current-index origin-index
                                    &optional prefix suffix)
  "Render LABELS as a fixed-width rail around CURRENT-INDEX.
ORIGIN-INDEX marks an origin slot.
PREFIX and SUFFIX are optional styled strings rendered outside the rail."
  (let* ((count (length labels))
         (layout (mjolnir--rail-layout count
                                       (window-width (minibuffer-window))
                                       prefix
                                       suffix))
         (slot-count (car layout))
         (slot-width (cdr layout))
         (indices (mjolnir--visible-ring-indices count current-index slot-count))
         (continuationp (> count slot-count))
         (separator (propertize
                     (format " %s " (mjolnir--glyph 'separator))
                     'face 'mjolnir-echo-separator))
         (rail (concat
                (propertize (mjolnir--glyph 'rail-left)
                            'face (if continuationp
                                      'mjolnir-echo-overflow
                                    'mjolnir-echo-separator))
                " "
                (mapconcat
                 (lambda (idx)
                   (mjolnir--render-slot
                    (nth idx labels)
                    slot-width
                    (= idx current-index)
                    (mjolnir--ring-distance count idx current-index)
                    (and origin-index (= idx origin-index))))
                 indices
                 separator)
                " "
                (propertize (mjolnir--glyph 'rail-right)
                            'face (if continuationp
                                      'mjolnir-echo-overflow
                                    'mjolnir-echo-separator)))))
    (concat
     (if prefix
         (concat prefix (propertize "  " 'face 'mjolnir-echo-meta))
       "")
     rail
     (if suffix
         (concat (propertize "  " 'face 'mjolnir-echo-meta) suffix)
       ""))))

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
      (mjolnir--show-window-numbers info))))

(defun mjolnir--show-window-numbers (info)
  "Show overlay numbers derived from INFO."
  (mjolnir--clear-overlays)
  (let ((windows (plist-get info :windows))
        (current-window (plist-get info :current-window))
        (origin-window (plist-get info :origin-window))
        (counter 1))
    (dolist (window windows)
      (when (window-live-p window)
        (let* ((label (number-to-string counter))
               (overlay (make-overlay 1 1 (window-buffer window))))
          (when (eq window origin-window)
            (setq label (concat "*" label)))
          (when (eq window current-window)
            (setq label (format "[%s]" label)))
          (overlay-put overlay 'window window)
          (overlay-put overlay 'priority 1000)
          (overlay-put overlay 'before-string
                       (propertize (format " %s " label)
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
