

;; #TODO:
(defmethod insertion-serial-heuristic-solution ((e entity-vrp-evaluator) (o entity-sample-vrp))
  "Answer an individual using insertion (serial version) heuristic."
  (let ((points)
        (initial-solution)
        (index)
        (vehicle-capacity (max-capacity e)))
    ;; Build points list
    (dotimes (i (length (cities-description e)))
      (appendf points (list i)))
    ;; Add first 3 random points
    (setf index (random-integer 0 (length points)))
    (appendf initial-solution (list (nth index points)))
    (removef points (nth index points))
    (setf index (random-integer 0 (length points)))
    (appendf initial-solution (list (nth index points)))
    (removef points (nth index points))
    (setf index (random-integer 0 (length points)))
    (appendf initial-solution (list (nth index points)))
    (removef points (nth index points))
    ;; Merge points
    (do ()
        ((null points))
      (dolist (i points)
        (if (< (+ capacity-value (capacity-for e i)) vehicle-capacity)
            (appendf candidates (list i (distance-between e (last initial-solution) i)))))
      nil)))

(defmethod capacity-for ((e entity-vrp-evaluator) city-index)
  "Answer capacity value for <e> at <city-index>."
  (aref (costs-matrix e) city-index))

;; #TODO:
(defmethod insertion-parallel-heuristic-solution ((e entity-vrp-evaluator) (o entity-sample-vrp))
  "Answer an individual using insertion (parallel version) heuristic."
  nil)
