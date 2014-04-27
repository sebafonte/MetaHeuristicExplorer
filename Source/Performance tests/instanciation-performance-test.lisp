
(defclass instanciation-performance-test (performance-test-case)
  ())


(defmethod performance-test-instanciation-search-task ((o instanciation-performance-test))
  (timed-performance-test 
   (lambda ()
     (dotimes (i 20)
       (make-instance 'search-task)))))

(defmethod performance-test-instanciation-pane-search-tasks ((o instanciation-performance-test))
  (timed-performance-test 
   (lambda ()
     (dotimes (i 20)
       (make-instance 'pane-search-tasks :open nil)))))
