
(defclass random-tree-creation-strategy (search-algorithm)
  ())


(defmethod create-individual ((a random-tree-creation-strategy) &optional (evaluate t))
  "Answer a new individual for <a>."
  (let* ((new-exp (create-expresion a (max-size a) (max-depth a) t nil))
         (simplified-exp (simplify-strategy (language a) new-exp a))
         (child (make-instance (objetive-class (context a)) :expresion simplified-exp)))
    (when evaluate (evaluate a child))
    child))