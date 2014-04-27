
(defun test-fitness-evaluator-x-speed (type)
  (let ((evaluator (make-instance 'entity-function-x-evaluator
                                  :name 'test-evaluator
                                  :description "Test evaluator"
                                  :samples 1000000
                                  :measure-start 0
                                  :measure-end 10
                                  :precision type))
        (object (make-instance 'entity-function-x :expresion '(+ 1 x (* x x) (* x x x) (* x x x x)))))
    (initialize-fitness-data evaluator)
    (time (evaluate-distance evaluator object))
    (time (evaluate-distance evaluator object))
    (time (evaluate-distance evaluator object))
    (time (evaluate-distance evaluator object))))

(defun test-fitness-evaluator-x-y-speed (type)
  (let ((evaluator (make-instance 'entity-function-x-y-evaluator
                                  :name 'test-evaluator
                                  :description "Test evaluator"
                                  :samples 1000
                                  :measure-start 0
                                  :measure-end 10
                                  :precision type))
        (object (make-instance 'entity-function-x :expresion '(+ 1 x (* x x) (* x x x) (* x x x x)))))
    (initialize-fitness-data evaluator)
    (time (evaluate-distance evaluator object))
    (time (evaluate-distance evaluator object))
    (time (evaluate-distance evaluator object))
    (time (evaluate-distance evaluator object))))
 
#|
(test-fitness-evaluator-x-speed 'float)
(test-fitness-evaluator-x-y-speed 'float)
|#