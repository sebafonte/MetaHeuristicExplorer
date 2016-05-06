(defclass test-population (test-base-model) ())


(defmethod default-algorithm ((o test-population) &optional &key (class 'generational-algorithm))
  "Answer default algorithm for <o>."
  (let* ((algorithm (make-instance class))
         (context (make-instance 'search-task :algorithm algorithm)))
    (setf (initialization-method algorithm) (copy-cyclic (system-get 'random-trees-cfg-initializer)))
    algorithm))

(defmethod test-normalize-population-fitness ((o test-population))
  "Test population fitness normalization."
  (let ((algorithm (default-algorithm o)))
    (generate-initial-population algorithm)
    (let ((population (population algorithm)))
      (normalize-population-fitness population #'fitness)
      (check (< (abs (- (apply '+ (mapcar #'fitness-normalized (individuals population)))
                     1.0))
                0.001)))))
