
(defclass default-task-global-execution-performance-test (performance-test-case)
  ())


(defmethod set-up ((o default-task-global-execution-performance-test))
  (reset-local-random-state o)
  (reset-external-random-state o))

(defmethod reset-local-random-state ((o default-task-global-execution-performance-test))
  (setf *seed* 1))

(defmethod reset-external-random-state ((o default-task-global-execution-performance-test))
  nil)

(defmethod performance-test-default-fxy-generational-task-1 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-fxy-generational-task-2 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-fxy-generational-task-3 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-vrp-task-1 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-vrp-task-2 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-vrp-task-3 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-search-task-task-1 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-search-task-task-2 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-search-task-task-3 ((o default-task-global-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))