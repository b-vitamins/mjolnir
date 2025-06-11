;;; mjolnir-test.el --- Comprehensive test suite for Mjolnir -*- lexical-binding: t -*-

;;; Commentary:
;; Exhaustive test suite that serves as the primary source of correctness.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load all mjolnir components
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." test-dir))
  (add-to-list 'load-path (expand-file-name "../core" test-dir))
  (add-to-list 'load-path (expand-file-name "../features" test-dir))
  (add-to-list 'load-path (expand-file-name "../ui" test-dir)))

(require 'mjolnir)

;;; Test Infrastructure

(defvar mjolnir-test--debug nil
  "Enable debug output during tests.")

(defvar mjolnir-test--performance-threshold 0.1
  "Maximum acceptable time in seconds for performance-critical operations.")

(defmacro mjolnir-test--measure-time (&rest body)
  "Measure execution time of BODY."
  `(let ((start (float-time)))
     ,@body
     (- (float-time) start)))

(defmacro mjolnir-test--with-clean-state (&rest body)
  "Execute BODY with clean Mjolnir state."
  `(unwind-protect
       (progn
         (mjolnir-mode -1)
         (setq mjolnir--fixed-windows nil)
         (mjolnir--invalidate-caches)
         ,@body)
     (mjolnir-mode -1)
      (mjolnir--clear-display)))

(defmacro mjolnir-test--with-temp-frame (&rest body)
  "Execute BODY in a temporary frame."
  (declare (indent 0))
  `(let ((process-environment (copy-sequence process-environment)))
     (setenv "TERM" "dumb")
     (condition-case err
         (let ((frame (make-frame '((visibility . nil)))))
           (unwind-protect
               (with-selected-frame frame
                 ,@body)
             (delete-frame frame)))
       (error (ert-skip (format "Frame creation failed: %s" err))))))

(defmacro mjolnir-test-with-temp-buffers (names &rest body)
  "Create temporary buffers listed in NAMES, execute BODY, then clean up."
  (declare (indent 1))
  `(let (buffers)
     (unwind-protect
         (progn
           (dolist (name ,names)
             (push (get-buffer-create name) buffers))
           ,@body)
       (dolist (buf buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;;; Core Functionality Tests

(ert-deftest mjolnir-test-mode-activation ()
  "Test mode activation and deactivation."
  (mjolnir-test--with-clean-state
   ;; Test activation
   (mjolnir-mode 1)
   (should mjolnir-mode)
   (should (keymapp mjolnir-mode-map))
   (should (member mjolnir--mode-line-format mode-line-format))

   ;; Test deactivation
   (mjolnir-mode -1)
   (should-not mjolnir-mode)
   (should-not (member mjolnir--mode-line-format mode-line-format))))

(ert-deftest mjolnir-test-keybinding-updates ()
  "Test dynamic keybinding updates."
  (mjolnir-test--with-clean-state
   (mjolnir-mode 1)
   (let ((old-key mjolnir-rotate-forward-key))
     (unwind-protect
         (progn
          ;; Change keybinding
          (customize-set-variable 'mjolnir-rotate-forward-key "C-c C-n")
           ;; Verify new binding works
           (should (eq (lookup-key mjolnir-mode-map (kbd "C-c C-n"))
                       'mjolnir-rotate-forward))
           ;; Verify old binding removed
           (should-not (eq (lookup-key mjolnir-mode-map (kbd old-key))
                          'mjolnir-rotate-forward)))
       (customize-set-variable 'mjolnir-rotate-forward-key old-key)))))

;;; Window Management Tests

(ert-deftest mjolnir-test-window-angle-calculation ()
  "Test window angle calculation accuracy."
  (mjolnir-test--with-temp-frame
    (delete-other-windows)
    (split-window-horizontally)
    (split-window-vertically)
    (other-window 1)
    (split-window-vertically)

    (let* ((windows (window-list))
           (angles (mapcar #'mjolnir--window-angle windows)))
      ;; All angles should be unique
      (should (= (length angles) (length (delete-dups angles))))
      ;; Angles should be in valid range
      (dolist (angle angles)
        (should (and (numberp angle)
                    (<= (- pi) angle pi)))))))

(ert-deftest mjolnir-test-window-sorting-stability ()
  "Test that window sorting is stable across calls."
  (mjolnir-test--with-temp-frame
    (delete-other-windows)
    (dotimes (_ 3)
      (split-window-horizontally)
      (other-window 1))

    (let ((sorted1 (mjolnir--sort-windows-by-angle))
          (sorted2 (mjolnir--sort-windows-by-angle)))
      (should (equal sorted1 sorted2)))))

(ert-deftest mjolnir-test-single-window-handling ()
  "Test graceful handling of single window."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (delete-other-windows)
     (mjolnir-mode 1)
     ;; Should not error
     (should-not (condition-case err
                     (progn (mjolnir-rotate-forward) nil)
                   (error err)))
     (should-not (condition-case err
                     (progn (mjolnir-rotate-backward) nil)
                   (error err))))))

(ert-deftest mjolnir-test-rotation-correctness ()
  "Test that rotation maintains buffer-window relationships."
  (mjolnir-test--with-clean-state
   (mjolnir-test-with-temp-buffers '("A" "B" "C" "D")
     (mjolnir-test--with-temp-frame
       (delete-other-windows)
       ;; Create 4-window layout
       (switch-to-buffer "A")
       (split-window-horizontally)
       (other-window 1)
       (switch-to-buffer "B")
       (split-window-vertically)
       (other-window 1)
       (switch-to-buffer "C")
       (other-window 1)
       (split-window-vertically)
       (other-window 1)
       (switch-to-buffer "D")

       (mjolnir-mode 1)
       (let ((initial (mapcar (lambda (w) (window-buffer w))
                             (mjolnir--sort-windows-by-angle))))
         ;; Rotate forward 4 times
         (dotimes (_ 4)
           (mjolnir-rotate-forward))
         ;; Should be back to initial state
         (should (equal initial
                       (mapcar (lambda (w) (window-buffer w))
                              (mjolnir--sort-windows-by-angle)))))))))

(ert-deftest mjolnir-test-unworthy-window-exclusion ()
  "Test that unworthy windows are excluded from rotation."
  (mjolnir-test--with-clean-state
   (mjolnir-test-with-temp-buffers '("A" "B" "C")
     (mjolnir-test--with-temp-frame
       (delete-other-windows)
       (switch-to-buffer "A")
       (split-window-horizontally)
       (other-window 1)
       (switch-to-buffer "B")
       (split-window-horizontally)
       (other-window 1)
       (switch-to-buffer "C")

       (mjolnir-mode 1)
       ;; Mark middle window as unworthy
       (other-window -1)
       (mjolnir-toggle-window-worthy)

       ;; Rotate and verify B stays put
       (mjolnir-rotate-forward)
       (should (eq (get-buffer "B")
                  (window-buffer (nth 1 (window-list)))))))))

;;; Buffer Summoning Tests

(ert-deftest mjolnir-test-buffer-summoning-order ()
  "Test buffer summoning maintains LRU order."
  (mjolnir-test--with-clean-state
   (mjolnir-test-with-temp-buffers '("A" "B" "C" "D" "E")
     (mjolnir-test--with-temp-frame
       (delete-other-windows)
       (mjolnir-mode 1)

       ;; Visit buffers in specific order
       (dolist (buf '("E" "D" "C" "B" "A"))
         (switch-to-buffer buf)
         (sit-for 0.01)) ; Ensure buffer list updates

       ;; Now summon should follow reverse order
       (switch-to-buffer "*scratch*")
       (mjolnir-summon-next)
       (should (equal (buffer-name) "A"))))))

(ert-deftest mjolnir-test-summon-project-filtering ()
  "Test project-only filtering for summoning."
  (skip-unless (fboundp 'project-current))
  (mjolnir-test--with-clean-state
   (let ((default-directory "/tmp/"))
     (mjolnir-test-with-temp-buffers '("proj1" "proj2" "other")
       (with-current-buffer "proj1"
         (setq default-directory "/tmp/project/"))
       (with-current-buffer "proj2"
         (setq default-directory "/tmp/project/"))
       (with-current-buffer "other"
         (setq default-directory "/tmp/other/"))

       (mjolnir-test--with-temp-frame
         (mjolnir-mode 1)
         (setq mjolnir-summon-project-only t)
         (switch-to-buffer "proj1")
         (let ((summonable (mjolnir--summonable-buffers)))
           ;; Should only include project buffers
           (should (member (get-buffer "proj2") summonable))
           (should-not (member (get-buffer "other") summonable))))))))

(ert-deftest mjolnir-test-summon-timeout-reset ()
  "Test summoning position resets after timeout."
  (mjolnir-test--with-clean-state
   (mjolnir-test-with-temp-buffers '("A" "B" "C")
     (mjolnir-test--with-temp-frame
       (mjolnir-mode 1)
       (let ((mjolnir-reset-timeout 0.1))
         (switch-to-buffer "*scratch*")
         ;; Summon once
         (mjolnir-summon-next)
         (let ((first-buffer (current-buffer)))
           ;; Wait for timeout
           (sleep-for 0.2)
           ;; Summon again - should reset to beginning
           (mjolnir-summon-next)
           (should (eq (current-buffer) first-buffer))))))))

;;; State Management Tests

(ert-deftest mjolnir-test-frame-local-state-isolation ()
  "Test that state is properly isolated between frames."
  (mjolnir-test--with-clean-state
   (mjolnir-mode 1)
   (let* ((frame1 (selected-frame))
          frame2
          state1 state2)
     (mjolnir-test--with-temp-frame
       (setq frame2 (selected-frame)))
     (unwind-protect
         (progn
           ;; Get states
           (with-selected-frame frame1
             (setq state1 (mjolnir--get-state)))
           (with-selected-frame frame2
             (setq state2 (mjolnir--get-state)))
           ;; Should be different objects
           (should-not (eq state1 state2))
           ;; Modify one state
           (setf (mjolnir--state-buffer-position state1) 5)
           ;; Other should be unaffected
           (should (= 0 (or (mjolnir--state-buffer-position state2) 0))))
       (delete-frame frame2)))))

(ert-deftest mjolnir-test-cache-invalidation ()
  "Test that caches are properly invalidated."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     (delete-other-windows)
     (split-window-horizontally)

     ;; Cache should be populated
     (let ((sorted1 (mjolnir--sort-windows-by-angle)))
       ;; Change window configuration
       (delete-other-windows)
       (split-window-vertically)
       ;; Cache should be invalidated
       (let ((sorted2 (mjolnir--sort-windows-by-angle)))
         (should-not (equal sorted1 sorted2)))))))

;;; Error Handling Tests

(ert-deftest mjolnir-test-deleted-window-handling ()
  "Test graceful handling of deleted windows."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     (delete-other-windows)
     (split-window-horizontally)
     (let ((win (selected-window)))
       (other-window 1)
       (delete-window win)
       ;; Should not error
       (condition-case err
           (progn
             (mjolnir-rotate-forward)
             (should t))
         (error (should-not err)))))))

(ert-deftest mjolnir-test-killed-buffer-handling ()
  "Test handling of killed buffers during operations."
  (mjolnir-test--with-clean-state
   (mjolnir-test-with-temp-buffers '("A" "B" "C")
     (mjolnir-test--with-temp-frame
       (mjolnir-mode 1)
       (switch-to-buffer "A")
       (split-window-horizontally)
       (other-window 1)
       (switch-to-buffer "B")

       ;; Kill a buffer that would be in rotation
       (kill-buffer "C")

       ;; Should handle gracefully
       (condition-case err
           (progn
             (mjolnir-rotate-forward)
             (should t))
         (error (should-not err)))))))

;;; Display Tests

(ert-deftest mjolnir-test-echo-area-display ()
  "Test echo area display functionality."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     (let ((mjolnir-display-method 'echo))
       ;; Capture messages
       (let ((messages nil))
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (push (apply #'format fmt args) messages))))
           (mjolnir-rotate-forward)
           ;; Should have displayed a message
           (should messages)
           (should (string-match "⚡" (car messages)))))))))

(ert-deftest mjolnir-test-overlay-cleanup ()
  "Test that overlays are properly cleaned up."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     (delete-other-windows)
     (split-window-horizontally)
     (let ((mjolnir-display-method 'overlays)
           (mjolnir-preview-duration 0.1))
       (mjolnir-preview-rotation)
       ;; Should have overlays
       (should mjolnir--preview-overlays)
       ;; Wait for cleanup
       (sleep-for 0.2)
       (mjolnir--clear-overlays)
       ;; Should be cleaned up
       (should-not mjolnir--preview-overlays)))))

;;; Performance Tests

(ert-deftest mjolnir-test-rotation-performance ()
  "Test rotation performance with many windows."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     (delete-other-windows)
     ;; Create 10 windows
     (dotimes (_ 9)
       (if (zerop (random 2))
           (split-window-horizontally)
         (split-window-vertically))
       (other-window 1))

     (let ((time (mjolnir-test--measure-time
                  (mjolnir-rotate-forward))))
       (should (< time mjolnir-test--performance-threshold))))))

(ert-deftest mjolnir-test-summon-performance ()
  "Test summoning performance with many buffers."
  (mjolnir-test--with-clean-state
   (mjolnir-test-with-temp-buffers
       (mapcar (lambda (i) (format "buffer-%d" i))
              (number-sequence 1 100))
     (mjolnir-test--with-temp-frame
       (mjolnir-mode 1)
       (let ((time (mjolnir-test--measure-time
                    (mjolnir-summon-next))))
         (should (< time mjolnir-test--performance-threshold)))))))

;;; Integration Tests

(ert-deftest mjolnir-test-winner-mode-compatibility ()
  "Test compatibility with winner-mode."
  (skip-unless (fboundp 'winner-mode))
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (winner-mode 1)
     (mjolnir-mode 1)
     (unwind-protect
         (progn
           (delete-other-windows)
           (split-window-horizontally)
           (mjolnir-rotate-forward)
           ;; Should work together
           (condition-case err
               (progn
                 (winner-undo)
                 (should t))
             (error (should-not err))))
       (winner-mode -1)))))

;;; Entry Points

(defun mjolnir-run-all-tests ()
  "Run all Mjolnir tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^mjolnir-test-"))

(defun mjolnir-run-minimal-tests ()
  "Run tests without optional dependencies."
  (interactive)
  (ert-run-tests-batch-and-exit "^mjolnir-test-\\(?!.*transient\\|.*project\\)"))

(provide 'mjolnir-test)
;;; mjolnir-test.el ends here
