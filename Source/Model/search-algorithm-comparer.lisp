
(defclass search-algorithm-comparer ()
  ())


(defmethod fitness-properties ((o search-algorithm-comparer))
  nil)

(defmethod abstractp ((o (eql 'search-algorithm-comparer)))
  "Answer whether <o> is abstract."
  (eql o 'search-algorithm-comparer))


(defclass evolvable-algorithm-comparer ()
  ())


(defmethod fitness-properties :after ((o evolvable-algorithm-comparer))
  (list
   ;; Fitness
   (best-fitness)
   (medium-fitness)
   (minimum-fitness)
   (frequency-best-fitness)
   (std-fitness)
   ;; Size
   (best-size)
   (medium-size)
   (maximum-size)
   (std-size)
   ;; Repetidos
   (repeat-count)
   (repeat-groups-count)
   ;; Diversity
   (root-nodes-count)
   (root-nodes-std)
   ;; Operators
   (operator-sucess-rate)))

