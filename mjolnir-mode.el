;;; mjolnir-mode.el --- Whosoever holds this hammer, if he be worthy, shall possess the power of Thor.

;; Author: B and ChatGPT
;; Version: 0.2.0
;; Keywords: buffers, windows, lightning, power, Thor, Mjolnir
;; URL: https://github.com/b-vitamins/mjolnir-mode

;;; Commentary:

;; `mjolnir-mode` is an Emacs minor mode that bestows upon you the might of Thor!
;;
;; When wielding Mjölnir, nothing shall come in the way of your buffers as they thunder through your windows.  Instead of moving over to the window holding the buffer worthy of your attention,  summon it into the window you're already in.  However, not all buffers you deem worthy - let be them smitten under the might of Mjölnir - and they shall stay their ground.
;;
;; *May your windows be ever steadfast and your code mighty!* ⚡

;;; Installation:

;; Clone this repository and add it to your `load-path`.
;;
;; (add-to-list 'load-path "/path/to/mjolnir-mode")
;; (require 'mjolnir-mode)

;;; Usage:

;; Enable `mjolnir-mode`:
;; (mjolnir-mode t)
;;
;; Now wield the power of Mjölnir wherever you are (`*`).
;;
;; Go this way (`M-n`):
;;
;; +----+---+    +----+---+    +----+---+    +----+---+
;; | A* | B |    | B* | C |    | C* | D |    | D* | A |
;; +----+---+ -> +----+---+ -> +----+---+ -> +----+---+
;; | D  | C |    | A  | D |    | B  | A |    | C  | B |
;; +----+---+    +----+---+    +----+---+    +----+---+
;;
;; Or that way (`M-p`):
;;
;; +----+---+    +----+---+    +----+---+    +----+---+
;; | A* | B |    | D* | A |    | C* | D |    | B* | C |
;; +----+---+ -> +----+---+ -> +----+---+ -> +----+---+
;; | D  | C |    | C  | B |    | B  | A |    | A  | D |
;; +----+---+    +----+---+    +----+---+    +----+---+
;;
;; Make `X` unworthy (`C-c u`), then go this way (`M-n`):
;;
;; +----+---+    +----+---+    +----+---+    +----+---+
;; | A* | B |    | B* | C |    | C* | A |    | A* | B |
;; +----+---+ -> +----+---+ -> +----+---+ -> +----+---+
;; | X  | C |    | X  | A |    | X  | B |    | X  | C |
;; +----+---+    +----+---+    +----+---+    +----+---+
;;
;; Change your grip using `mjolnir-cycle-window-forward-key`, `mjolnir-cycle-window-backward-key`, and `mjolnir-toggle-fixed-window-key`.

;;; Code:

(require 'seq)

(defgroup mjolnir nil
  "Customization group for mjolnir-mode."
  :prefix "mjolnir-"
  :group 'convenience)

(defcustom mjolnir-fixed-modes '(treemacs-mode eldoc-mode)
  "List of modes whose windows shall not cycle buffers, deemed unworthy of Mjölnir."
  :type '(repeat symbol)
  :group 'mjolnir)

(defcustom mjolnir-cycle-window-forward-key "M-n"
  "Key binding for cycling the next buffer forward in the current window."
  :type 'string
  :group 'mjolnir)

(defcustom mjolnir-cycle-window-backward-key "M-p"
  "Key binding for cycling the next buffer backward in the current window."
  :type 'string
  :group 'mjolnir)

(defcustom mjolnir-toggle-fixed-window-key "C-c u"
  "Key binding for toggling current window's eligibility in mjolnir-mode."
  :type 'string
  :group 'mjolnir)

(defvar mjolnir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd mjolnir-cycle-window-forward-key) 'mjolnir-cycle-window-forward)
    (define-key map (kbd mjolnir-cycle-window-backward-key) 'mjolnir-cycle-window-backward)
    (define-key map (kbd mjolnir-toggle-fixed-window-key) 'mjolnir-toggle-fixed-window)
    map)
  "Keymap for mjolnir-mode.")

(defvar mjolnir-frame-center-cache nil
  "Cache holding frame center coordinates to optimize window angle calculations.")

(defvar mjolnir-sorted-windows-cache nil
  "Cached list of sorted windows according to their angles from frame center.")

(defvar mjolnir-window-angle-cache (make-hash-table :test 'equal)
  "Cache storing window angles to optimize sorting.
Keys are window objects; values are angles.")

(defvar mjolnir-fixed-windows '()
  "List of windows that are not subject to buffer cycling.")

(defvar mjolnir-mode-load-hook nil
  "Hook run after the mjolnir-mode package is loaded.")

;;;###autoload
(define-minor-mode mjolnir-mode
  "Toggle mjolnir-mode globally.
Enables dynamic buffer cycling across windows,
with specific windows designated as `unworthy' remaining static."
  :lighter " Mjölnir"
  :global t
  :keymap mjolnir-mode-map
  (if mjolnir-mode
      (progn
        (add-hook 'window-configuration-change-hook 'mjolnir-clear-cache)
        (message "Mjölnir mode enabled globally."))
    (progn (remove-hook 'window-configuration-change-hook 'mjolnir-clear-cache)
           (message "Mjölnir mode disabled globally."))))

(defun mjolnir-cycle-window-forward ()
  "Cycle the next buffer forward in the current window."
  (interactive)
  (mjolnir-cycle-windows 'forward))

(defun mjolnir-cycle-window-backward ()
  "Cycle the next buffer backward in the current window."
  (interactive)
  (mjolnir-cycle-windows 'backward))

(defun mjolnir-cycle-windows (direction)
  "Cycle buffers in visible windows in DIRECTION."
  (interactive)
  (unless (member direction '(forward backward))
    (user-error "Invalid cycling direction: %s" direction))
  (let* ((windows (delq nil (mapcar
                             (lambda (w)
                               (unless (or (member w mjolnir-fixed-windows)
                                           (member (buffer-local-value
                                                    'major-mode (window-buffer w))
                                                   mjolnir-fixed-modes))
                                 w))
                             (window-list))))
         (sorted-windows (mjolnir-sort-windows))
         (eligible-windows (seq-filter (lambda (w) (member w windows)) sorted-windows))
         (buffers (mapcar 'window-buffer eligible-windows))
         (num-windows (length eligible-windows))
         (offset (if (eq direction 'forward) -1 1)))
    (when (> num-windows 1)
      (dotimes (i num-windows)
        (let ((target-index (mod (+ i offset) num-windows)))
          (set-window-buffer (nth i eligible-windows)
                             (nth target-index buffers)))))))

(defun mjolnir-clear-cache ()
  "Clear all mjolnir caches.
Call this function on window configuration changes."
  (interactive)
  (clrhash mjolnir-window-angle-cache)
  (setq mjolnir-sorted-windows-cache nil))

(defun mjolnir-window-changed-p (window)
  "Check if WINDOW's geometry has changed since last caching."
  (let ((last-recorded (gethash window mjolnir-window-angle-cache))
        (current-angle (mjolnir-calculate-window-angle window t)))
    (not (equal last-recorded current-angle))))

(defun mjolnir-update-frame-center-cache (frame)
  "Update FRAME center coordinates cache."
  (setq mjolnir-frame-center-cache
        (cons (/ (frame-pixel-width frame) 2.0)
              (/ (frame-pixel-height frame) 2.0))))

(defun mjolnir-sort-windows ()
  "Sort windows and update cache if necessary."
  (unless mjolnir-sorted-windows-cache
    (setq mjolnir-sorted-windows-cache
          (sort (window-list) (lambda (a b)
                                (< (mjolnir-calculate-window-angle a)
                                   (mjolnir-calculate-window-angle b))))))
  mjolnir-sorted-windows-cache)

(defun mjolnir-calculate-window-angle (window &optional force-recalculate)
  "Return the angle from FRAME's center to WINDOW's center.
Optionally FORCE-RECALCULATE the angle."
  (unless (eq (window-frame window) (car mjolnir-frame-center-cache))
    (mjolnir-update-frame-center-cache (window-frame window)))
  (if (and (not force-recalculate) (gethash window mjolnir-window-angle-cache))
      (gethash window mjolnir-window-angle-cache)
    (let* ((edges (window-edges window nil nil t))
           (center (cons (/ (+ (nth 0 edges) (nth 2 edges)) 2.0)
                         (/ (+ (nth 1 edges) (nth 3 edges)) 2.0)))
           (dx (- (car center) (car mjolnir-frame-center-cache)))
           (dy (- (cdr mjolnir-frame-center-cache) (cdr center)))
           (angle (atan dy dx)))
      (puthash window angle mjolnir-window-angle-cache)
      angle)))

(defun mjolnir-toggle-fixed-window ()
  "Toggle current window's eligibility for buffer cycling."
  (interactive)
  (if (memq (selected-window) mjolnir-fixed-windows)
      (progn
        (setq mjolnir-fixed-windows (delq (selected-window) mjolnir-fixed-windows))
        (message "Window is now worthy."))
    (add-to-list 'mjolnir-fixed-windows (selected-window))
    (message "Window is now unworthy.")))

;;;###autoload
(when (featurep 'mjolnir-mode)
  (run-hooks 'mjolnir-mode-load-hook))

(provide 'mjolnir-mode)
;;; mjolnir-mode.el ends here