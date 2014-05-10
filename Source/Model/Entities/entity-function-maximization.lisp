
(defclass entity-function-maximization (entity)
  ())


(defmethod default-language ((o entity-function-maximization))
  (system-get 'binary-language-32))

(defmethod possible-languages ((o entity-function-maximization))
  (list 
   (system-get 'binary-language-32)
   (system-get 'binary-language-64)))

(defmethod default-fitness-evaluators ((o entity-function-maximization))
  "Answer the default classes that can evaluate <o> fitness."
  (list 
   (system-get 'function-maximization-evaluator)))

(defmethod default-population-initializer ((o entity-function-maximization))
  "Answer the default population initializer for <o>."
  (system-get 'random-number-initializer))

(defmethod drawablep ((o entity-function-maximization))
  "Answer whether <o> can be displayed on the GUI."
  t)
