
(defclass object-evaluation-performance-test (performance-test-case)
  ())


(defmethod performance-test-entity-function-x ((o object-evaluation-performance-test))
  (let ((evaluator (make-instance 'entity-function-x-evaluator
                                  :name 'evaluator-for-test
                                  :description ""
                                  :samples 50000
                                  :measure-start 0
                                  :measure-end 10))
        (object (make-instance 'entity-function-x :expresion '(+ (+ (/- x (/- x (+ (* x (sin x)) (+ x x)))) 
                                                                            (- (cos x) x)) 
                                                                         (+ (* x (sin x)) (+ x x))))))
    (initialize-fitness-data evaluator)
    (timed-performance-test 
     (lambda ()
       (evaluate evaluator object)))))

(defmethod performance-test-entity-function-x-y ((o object-evaluation-performance-test))
  (let ((evaluator (make-instance 'entity-function-x-y-evaluator
                                  :name 'evaluator-for-test
                                  :description ""
                                  :samples 400
                                  :measure-start 0
                                  :measure-end 10))
        (object (make-instance 'entity-function-x-y :expresion '(+ (+ (/- x (+ (* x (sin y)) (+ x y)))
                                                                            (- (cos x) x)) 
                                                                         (+ (* x (sin y)) (+ x y))))))
    (initialize-fitness-data evaluator)
    (timed-performance-test 
     (lambda ()
       (evaluate evaluator object)))))

(defmethod performance-test-entity-sample-vrp ((o object-evaluation-performance-test))
  (let ((evaluator (system-get 'entity-sample-vrp-evaluator))
        (object (make-instance 'entity-sample-vrp)))
    (timed-performance-test 
     (lambda ()
       (evaluate evaluator object)))))

(defmethod performance-test-entity-lop ((o object-evaluation-performance-test))
  (let ((evaluator (system-get 'entity-linear-ordering-evaluator))
        (object (make-instance 'entity-linear-ordering)))
    (timed-performance-test 
     (lambda ()
       (evaluate evaluator object)))))

