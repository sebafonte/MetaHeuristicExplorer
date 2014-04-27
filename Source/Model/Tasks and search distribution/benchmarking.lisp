(defparameter *default-configuration-path-benchmark-transfer-file* nil)


(defun timed-performance-test (function-name)
  (let ((initial-time (get-internal-real-time)))
    (funcall function-name)
    (- (get-internal-real-time) initial-time)))

(defun performance-test-function-1 ()
  (let ((x 123))
    (dotimes (i 1000)
      (setf x (/- 150 (- (* x i 3) (* x 50) (+ i i i 2)))))))

(defun create-sample-performance-test-file ()
  (car (load-from-file *default-configuration-path-benchmark-transfer-file*)))



