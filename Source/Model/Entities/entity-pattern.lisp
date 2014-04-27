(defclass entity-pattern (entity)
  ())


(defmethod default-fitness-evaluators ((o entity-pattern))
  "Answer the default classes that can evaluate <o> fitness."
  (system-get 'entity-pattern-evaluator))


  