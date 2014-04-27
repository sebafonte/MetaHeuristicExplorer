
(defclass environment-object-transfer-performance-test (performance-test-case)
  ())

(defmethod set-up ((o environment-object-transfer-performance-test))
  (reset-local-random-state o)
  (reset-external-random-state o))

(defmethod reset-local-random-state ((o environment-object-transfer-performance-test))
  (setf *seed* 1))

(defmethod reset-external-random-state ((o environment-object-transfer-performance-test))
  nil)

(defmethod performance-test-local-image-transfer ((o environment-object-transfer-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-local-image-execution ((o environment-object-transfer-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-environment-transfer ((o environment-object-transfer-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-environment-execution ((o environment-object-transfer-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-external-environment-execution ((o environment-object-transfer-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))