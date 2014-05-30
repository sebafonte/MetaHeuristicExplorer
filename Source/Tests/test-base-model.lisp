
(defclass test-base-model (test-case) 
  ())


(defmethod default-algorithm ((o test-base-model) &optional &key (class 'generational-algorithm))
  "Answer default algorithm for <o>."
  (let* ((algorithm (make-instance class))
         (context (make-instance 'search-task :algorithm algorithm)))
    algorithm))