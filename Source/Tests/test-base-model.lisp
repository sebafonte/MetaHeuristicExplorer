
(defclass test-base-model (test-case) 
  ())


(defmethod default-algorithm ((o test-base-model) &optional &key (class 'generational-algorithm))
  "Answer default algorithm for <o>."
  (let* ((algorithm (make-instance class))
         (context (make-instance 'search-task :algorithm algorithm)))
    (initialize-properties algorithm)
    (initialize-properties context)
    (initialize-properties-for context algorithm)
    (setf (context algorithm) context)
    algorithm))