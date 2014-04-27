(defclass test-population (test-base-model) ())


(defmethod test-normalize-population-fitness ((o test-population))
  "Test population fitness normalization."
  (let ((algorithm (default-algorithm o)))
    (mp:process-wait "Waiting for test data generation..."
                     (lambda () (generate-initial-population algorithm)))
    (let ((population (population algorithm)))
      (normalize-population-fitness population #'fitness)
      (check (< (abs (- (apply '+ (mapcar #'fitness-normalized (individuals population)))
                     1.0))
                0.001)))))
