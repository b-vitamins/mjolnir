;;; mjolnir-mode.el --- Whosoever holds this hammer, if he be worthy, shall possess the power of Thor.

;; Author: B and ChatGPT
;; Version: 0.1.0
;; Keywords: buffers, windows, lightning, power, Thor, Mjolnir
;; URL: https://github.com/b-vitamins/mjolnir-mode

;;; Commentary:

;; `mjolnir-mode` is an Emacs minor mode that bestows upon you the might of Thor!
;; When wielding Mjölnir,  nothing shall come in the way of your buffers as they thunder through the windows.  However, not all buffers are deemed worthy - the unworthy shall remain ;; immobile, having collapsed under the weight of the mighty Mjölnir.

;;; Usage:

;; (require 'mjolnir-mode)
;;
;; To wield the power of Mjölnir, enable `mjolnir-mode`:
;; (mjolnir-mode t)
;;
;; To journey all of the realms with it, enable `global-mjolnir-mode`:
;; (global-mjolnir-mode t)

;;; Code:

(defgroup mjolnir nil
  "Customization group for mjolnir-mode."
  :group 'convenience
  :prefix "mjolnir-")

(defcustom mjolnir-fixed-modes '(treemacs-mode eldoc-mode)
  "Modes whose windows shall not cycle buffers, deemed unworthy of Mjölnir."
  :type '(repeat symbol)
  :group 'mjolnir)

(defcustom mjolnir-cycle-window-keys '("M-n" . "M-p")
  "Cons cell where:
1) car is the key for cycling forward.
2) cdr is for cycling backward."
  :type 'cons
  :group 'mjolnir)

(defcustom mjolnir-toggle-fixed-window-key "M-t"
  "Key binding for toggling current window's eligibility in mjolnir-mode."
  :type 'string
  :group 'mjolnir)

(defvar mjolnir-frame-center-cache nil
  "Cache holding frame center coordinates to optimize calculations.")

(defvar mjolnir-sorted-windows-cache nil
  "Cached list of sorted windows according to their angles.")

(defvar mjolnir-window-angle-cache (make-hash-table :test 'equal)
  "Cache storing window angles.
Keys are window objects; values are angles.")

(defvar mjolnir-mode-load-hook nil
  "Hook run after the mjolnir-mode package is loaded.")

(defvar mjolnir-load-hook nil
  "Hook run after mjolnir is loaded.")

(defvar mjolnir-fixed-windows '()
  "List of windows that are not subject to buffer cycling.")

(defvar mjolnir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd (car mjolnir-cycle-window-keys)) #'mjolnir-cycle-window-forward)
    (define-key map (kbd (cdr mjolnir-cycle-window-keys)) #'mjolnir-cycle-window-backward)
    map)
  "Keymap for mjolnir-mode.")

;;;###autoload
(define-minor-mode mjolnir-mode
  "Toggle mjolnir-mode.
This mode enables dynamic buffer cycling across windows."
  :lighter " Mjölnir"
  :global nil
  :keymap mjolnir-mode-map
  (if mjolnir-mode
      (add-hook 'window-configuration-change-hook #'mjolnir-clear-cache)
    (remove-hook 'window-configuration-change-hook #'mjolnir-clear-cache)))

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
    (error "No such realm: %s" direction))
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

(provide 'mjolnir-mode)
;;; mjolnir-mode.el ends here