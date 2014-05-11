
(defclass random-tree-creation-strategy (search-algorithm)
  ())


(defmethod create-individual ((a random-tree-creation-strategy) &optional (evaluate t))
  "Answer a new individual for <a>."
  (let* ((new-exp (create-expresion a (max-size a) (max-depth a) t nil))
         (simplified-exp (simplify-strategy (language a) new-exp a))
         (child (make-instance (objetive-class (context a)) :expresion simplified-exp)))
    (when evaluate (evaluate a child))
    child))

(defmethod set-defaults-for-objetive ((a random-tree-creation-strategy))
  "Set genetic operators of <a> from a default instance in it's context."
  (let ((instance (objetive-instance (context a))))
    (setf (initialization-method a) (default-population-initializer instance))))