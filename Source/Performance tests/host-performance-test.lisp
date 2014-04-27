
(defclass host-performance-test (performance-test-case)
  ())

(defmethod performance-test-host-benchmark-math-1 ((o host-performance-test))
  (timed-performance-test 
   (lambda ()
     (let ((result 0))
       (dotimes (i 10000000)
         (setf result (+ result
                         (* result (- result 13 result 55553 result))
                         (* result (- result 3 result 5555 result) 
                            2 result result 3 4 result)
                         (* result (- result 11233 result 555353 result))
                         (* result (- result 1233 result 55515 result) 
                            2 (+ result
                                 (* result (- result 13 result 55553 result))
                                 (* result (- result 3 result 5555 result) 
                                    2 result result 3 4 result)
                                 (* result (- result 11233 result 555353 result))
                                 (* result (- result 1233 result 55515 result) 
                                    2 result result 1233 433 result)) result 1233 433 result))))))))

(defmethod performance-test-host-benchmark-math-2 ((o host-performance-test))
  (timed-performance-test 
   (lambda ()
     (let ((result 0))
       (dotimes (i 50000)
         (setf result (+ (- (tan result)
                            (* result (* 2.7776 
                                         (sin result)
                                         (+ 1.5 (cos result)))))
                         27.2)))))))

(defmethod performance-test-host-benchmark-memory ((o host-performance-test))
  (let ((matrix (make-array 10000 :initial-element 1)))
    (timed-performance-test 
     (lambda ()
       (dotimes (i 100000)
         (dotimes (i 1000)
           (setf (aref matrix i) (aref matrix (+ 1 i)))))))))