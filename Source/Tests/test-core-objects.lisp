(defclass test-core-objects (test-base-model) ())
(defclass test-entity (test-core-objects) ())
(defclass test-entity-x (test-core-objects) ())
(defclass test-entity-x-y (test-core-objects) ())
(defclass test-entity-image-rgb (test-core-objects) ())
(defclass test-entity-image-bw-seamless (test-core-objects) ())


(defmethod fitness-check ((o test-core-objects) evaluator a b max-fitness)
  "Check whether <a> fitness is less >= than <max-fitness> and <b> fitness is <."
  (check
    (>= (evaluate evaluator a) max-fitness)
    (< (evaluate evaluator b) max-fitness)))

(defmethod default-search-object ((test test-core-objects) &optional &key 
                                  (expression '(+ (* x x) 7)) 
                                  (class 'entity-function-x-y))
  "Answer a default object for <test>."
  (make-instance class :gen expression))

(defmethod test-reinitialize-properties-process-language ((o test-core-objects))
  "Verifies whether a algorithm reinitialize dependent properties for <o>."
  (let* ((algorithm (default-algorithm o))
         (context (context algorithm)))
    (setf (language context) (copy (language context)))
    (setf (slot-value (language context) 'operators) 'some-value)
    (setf (objetive-class context) 'entity-function-x)
    (check (not (equal 'some-value (operators (language context)))))))

(defmethod test-fitness-evaluation-case-comparison ((o test-entity-x))
  "Test basic behaviour for instance of class 'entity-function-x."
  (let ((a (make-instance 'entity-function-x :expresion '(+ (* x x) 7)))
        (b (make-instance 'entity-function-x :expresion '(+ (* x x) 8)))
        (evaluator (copy (system-get 'fine-0-10-function-x-evaluator))))
    (setf (target-program evaluator) '(+ (* x x) 7))
    (initialize-fitness-data evaluator)
    (fitness-check o evaluator a b (solution-fitness evaluator))))

(defmethod test-fitness-evaluation-case-comparison ((o test-entity-x-y))
  "Test basic behaviour for instance of class 'entity-function-x-y."
  (let ((a (make-instance 'entity-function-x-y :expresion '(+ (* x y) 7)))
        (b (make-instance 'entity-function-x-y :expresion '(+ (* x y) 8)))
        (evaluator (copy (system-get 'fine-0-10-function-x-y-evaluator))))
    (setf (target-program evaluator) '(+ (* x y) 7))
    (initialize-fitness-data evaluator)
    (fitness-check o evaluator a b (solution-fitness evaluator))))

(defmethod test-instanciation ((o test-entity-image-rgb))
  "Test basic behaviour for instance of class 'entity-image-rgb."
  (make-instance 'entity-image-rgb :expresion '(vec-* (vec-sin x) (vec-cos y))))
    
(defmethod test-instanciation ((o test-entity-image-bw-seamless))
  "Test basic behaviour for instance of class 'entity-image-bw-seamless."
  (make-instance 'entity-image-bw-seamless :expresion '(vec-* (vec-sin x) (vec-cos y))))
