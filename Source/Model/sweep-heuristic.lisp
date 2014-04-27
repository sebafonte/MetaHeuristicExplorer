
(defmethod sweep-heuristic-solution ((e entity-vrp-evaluator) (o entity-sample-vrp))
  "Answer an individual using sweep heuristic."
  nil)

(defmethod tour-mutate ((e entity-vrp-evaluator) (o entity-sample-vrp))
  nil)

(defmethod tour-local-search ((e entity-vrp-evaluator) (o entity-sample-vrp))
  nil)

(defmethod tour-split ((e entity-vrp-evaluator) (o entity-sample-vrp))
  nil)

(defmethod tour-concat ((e entity-vrp-evaluator) (o entity-sample-vrp))
  nil)
