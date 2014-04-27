
(defclass test-clark-and-wright (test-base-model)
  ())


(defmethod test-clark-and-wright-parallel-1 ((o test-clark-and-wright))
  (let ((evaluator (make-instance 'entity-vrp-evaluator :description "Test"))
        (object (make-instance 'entity-sample-vrp)))
    (setf (max-capacity evaluator) 100
          (cities-description evaluator) '(0 1 2 3 4 5)
          (demand-description evaluator) '(0 37 35 30 25 32)
          (demand-matrix evaluator) (to-array (demand-description evaluator))
          (costs-matrix evaluator)
          #2a((0   28  31  20  25  34)
              (28   0  21  29  26  20)
              (31  21   0  38  20  32)
              (20  29  38   0  30  27)
              (25  26  20  30   0  25)
              (34  20  32  27  25   0))
          (program object) (clark-and-wright-parallel evaluator))
    (evaluate evaluator object)
    (check 
      (= (fitness object) 171)
      (equal '((2 4) (3 5 1)) (program object)))))
