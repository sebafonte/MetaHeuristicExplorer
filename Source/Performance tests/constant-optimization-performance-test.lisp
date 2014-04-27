
(defclass constant-optimization-performance-test (performance-test-case test-base-model)
  ())


(defmethod performance-test-steepest-descent ((o constant-optimization-performance-test))
  (let* ((algorithm (default-algorithm o))
         (method (system-get 'optimization-method-steepest-descent))
         (entity (make-instance 'entity-function-x-y))
         (object (make-instance 'object-in-search :object entity :context (context algorithm)))
         (evaluator (fitness-evaluator algorithm)))
    (setf (target-program evaluator) '(* x 2000)
          (samples evaluator) 10
          (measure-start evaluator) 0
          (measure-end evaluator) 10)
    (initialize-fitness-data evaluator)
    (timed-performance-test 
     (lambda ()
       (dotimes (i 10)
         (setf (program (object object)) '(* x 1))
         (evaluate evaluator (object object))
         (execute-optimization-on method object))))))
