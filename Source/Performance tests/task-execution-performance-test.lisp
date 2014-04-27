
(defclass task-execution-performance-test (performance-test-case)
  ())


(defmethod performance-test-default-search-task-local-transfer ((o task-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))