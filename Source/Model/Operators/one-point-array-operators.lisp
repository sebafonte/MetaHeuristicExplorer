
(defclass one-point-array-crossover (binary-genetic-operator)
  ())


(defmethod operate ((o one-point-array-crossover) language exps)
  (let ((weight-function (production-selection-weight-function operator)))
    (directed-crossover-cfg 
     (create-random-from-production language '(start) (max-size language) weight-function)
     program
     language 
     operator)))


(defclass one-point-array-mutation (unary-genetic-operator)
  ())


(defmethod operate ((o one-point-array-mutation) language exps)
  (let ((weight-function (production-selection-weight-function operator)))
    (directed-crossover-cfg 
     (create-random-from-production language '(start) (max-size language) weight-function)
     program
     language 
     operator)))

