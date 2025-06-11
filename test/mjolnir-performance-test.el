;;; mjolnir-performance-test.el --- Performance tests for Mjolnir -*- lexical-binding: t -*-

;;; Commentary:
;; Performance regression tests.

;;; Code:

(require 'ert)
(require 'mjolnir-test)
(require 'benchmark)

(defvar mjolnir-performance-baselines
  '((rotation-10-windows . 0.01)
    (summon-100-buffers . 0.01)
    (angle-calculation . 0.001)
    (cache-rebuild . 0.005))
  "Performance baselines in seconds.")

(ert-deftest mjolnir-perf-rotation-scaling ()
  "Test rotation performance scales well with window count."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     (let ((timings nil))
       ;; Test with increasing window counts
       (dolist (count '(2 4 8 16))
         (delete-other-windows)
         (dotimes (_ (1- count))
           (split-window))
         (push (cons count
                    (benchmark-elapse
                      (mjolnir-rotate-forward)))
               timings))
       ;; Performance should scale sub-linearly
       (let ((ratio (/ (cdr (assq 16 timings))
                      (cdr (assq 2 timings)))))
         (should (< ratio 10)))))))

(ert-deftest mjolnir-perf-cache-effectiveness ()
  "Test cache effectiveness."
  (mjolnir-test--with-clean-state
   (mjolnir-test--with-temp-frame
     (mjolnir-mode 1)
     (delete-other-windows)
     (dotimes (_ 5)
       (split-window))
     ;; First call builds cache
     (mjolnir--sort-windows-by-angle)
     ;; Subsequent calls should be much faster
     (let ((cached-time
            (benchmark-elapse
              (dotimes (_ 100)
                (mjolnir--sort-windows-by-angle)))))
       (should (< cached-time 0.01))))))

(defun mjolnir-run-performance-tests ()
  "Run performance tests and compare to baselines."
  (interactive)
  (ert-run-tests-batch-and-exit "^mjolnir-perf-"))

(provide 'mjolnir-performance-test)
;;; mjolnir-performance-test.el ends here
