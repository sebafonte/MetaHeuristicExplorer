
(defclass subroutine-scoring-strategy (object-with-properties)
  ())

(defmethod score-population ((strategy subroutine-scoring-strategy) subtree population)
  (error "Subclass responsibility"))

(defmethod score-individuals ((strategy subroutine-scoring-strategy) subtree population)
  (error "Subclass responsibility"))


(defclass subroutine-equal-scoring-strategy (subroutine-scoring-strategy)
  ())

(defmethod score-population ((strategy subroutine-equal-scoring-strategy) subtree population)
  1)


(defclass subroutine-fitness-proportional-scoring-strategy (subroutine-scoring-strategy)
  ())

(defmethod score-population ((strategy subroutine-fitness-proportional-scoring-strategy) subtree population)
  (let ((accumulated-fitness 0))
    (dolist (i (individuals population))
      (when (includes-subtree (program i) subtree)
        (incf accumulated-fitness (fitness i))))
    accumulated-fitness))

(defmethod score-individuals ((strategy subroutine-fitness-proportional-scoring-strategy) subtree population)
  (let ((ocurrences))
    (dolist (i (individuals population))
      (appendf ocurrences (list (if (includes-subtree (program i) subtree) (fitness i) 0))))
    ocurrences))

(defclass subroutine-frequency-proportional-scoring-strategy (subroutine-scoring-strategy)
  ())

(defmethod score-population ((strategy subroutine-frequency-proportional-scoring-strategy) subtree population)
  (let ((ocurrences 0))
    (dolist (i (individuals population))
      (when (includes-subtree (program i) subtree)
        (incf ocurrences)))
    ocurrences))

(defmethod score-individuals ((strategy subroutine-frequency-proportional-scoring-strategy) subtree population)
  (let ((ocurrences))
    (dolist (i (individuals population))
      (appendf ocurrences (list (if (includes-subtree (program i) subtree) 1 0))))
    ocurrences))




