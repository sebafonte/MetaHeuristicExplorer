
(defclass default-task-local-execution-performance-test (performance-test-case)
  ())


(defmethod set-up ((o default-task-local-execution-performance-test))
  (reset-local-random-state o))

(defmethod reset-local-random-state ((o default-task-local-execution-performance-test))
  (setf *seed* 1))

;; Entities tiny search
(defmethod performance-test-example-fxy-generational-task ((o default-task-local-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-example-vrp-task ((o default-task-local-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

;; Tasks
(defmethod performance-test-search-task-instanciation ((o default-task-local-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-search-task-copy ((o default-task-local-execution-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

