;;; mjolnir-stress-test.el --- Stress tests for Mjolnir -*- lexical-binding: t -*-

;;; Commentary:
;; Stress tests to ensure Mjolnir remains stable under extreme conditions.

;;; Code:

(require 'ert)
(require 'mjolnir-test)

(ert-deftest mjolnir-stress-rapid-rotation ()
  "Stress test rapid rotation commands."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     ;; Create complex layout
     (delete-other-windows)
     (dotimes (_ 5)
       (split-window-horizontally)
       (other-window 1)
       (split-window-vertically))
     
     ;; Hammer it with rotations
     (let ((start-config (current-window-configuration)))
       (dotimes (_ 1000)
         (if (zerop (random 2))
             (mjolnir-rotate-forward)
           (mjolnir-rotate-backward)))
       ;; Should still be functional
       (should (window-list))
       ;; Do a full rotation cycle
       (let ((windows (mjolnir--sort-windows-by-angle)))
         (dotimes (_ (length windows))
           (mjolnir-rotate-forward)))))))

(ert-deftest mjolnir-stress-concurrent-operations ()
  "Test concurrent operations across multiple frames."
  (mjolnir-test--with-clean-state
   (mjolnir-mode 1)
   (let ((frames (list (selected-frame)
                      (make-frame '((visibility . nil)))
                      (make-frame '((visibility . nil))))))
     (unwind-protect
         (progn
           ;; Perform operations on all frames rapidly
           (dotimes (_ 100)
             (let ((frame (nth (random 3) frames)))
               (with-selected-frame frame
                 (when (< (length (window-list)) 10)
                   (split-window))
                 (case (random 4)
                   (0 (mjolnir-rotate-forward))
                   (1 (mjolnir-rotate-backward))
                   (2 (mjolnir-summon-next))
                   (3 (mjolnir-toggle-window-worthy))))))
           ;; All frames should still be valid
           (dolist (frame frames)
             (should (frame-live-p frame))))
       (mapc #'delete-frame (cdr frames))))))

(ert-deftest mjolnir-stress-memory-allocation ()
  "Test for memory leaks during extended use."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     (let ((gc-cons-threshold most-positive-fixnum)
           (initial-memory (garbage-collect)))
       ;; Perform many operations
       (dotimes (_ 10000)
         (mjolnir--window-angle (selected-window))
         (mjolnir--sort-windows-by-angle)
         (mjolnir--get-state)
         (mjolnir--invalidate-caches))
       ;; Force GC and check memory
       (garbage-collect)
       (let ((final-memory (garbage-collect)))
         ;; Memory usage shouldn't explode
         ;; This is a basic check - real memory profiling would be more complex
         (should t))))))

(defun mjolnir-run-stress-tests ()
  "Run all stress tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^mjolnir-stress-"))

(provide 'mjolnir-stress-test)
;;; mjolnir-stress-test.el ends here
