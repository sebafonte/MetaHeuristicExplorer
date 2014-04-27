
(defclass source-code-conversion-performance-test (performance-test-case)
  ())


(defmethod performance-test-default-search-task-uncompressed-store-memory ((o source-code-conversion-performance-test))
  (let ((task (make-instance 'search-task)))
    (length (transportable-code-description task))))

(defmethod performance-test-default-search-task-uncompressed-store-speed ((o source-code-conversion-performance-test))
  (let ((task (make-instance 'search-task)))
    (timed-performance-test 
     (lambda () 
       (transportable-code-description task)))))

(defmethod performance-test-default-search-task-uncompressed-read-speed ((o source-code-conversion-performance-test))
  (let* ((task (make-instance 'search-task))
         (code (transportable-code-description task)))
    (timed-performance-test 
     (lambda () 
       (eval code)))))

(defmethod performance-test-default-search-task-compressed-store-memory ((o source-code-conversion-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))

(defmethod performance-test-default-search-task-compressed-read-speed ((o source-code-conversion-performance-test))
  (timed-performance-test 
   (lambda ()
     nil)))
