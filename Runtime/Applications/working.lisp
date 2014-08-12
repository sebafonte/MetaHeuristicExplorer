(defmethod create-child ((o entity-function) algorithm operacion parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <o> geneotype."
  (let* ((program-list (mapcar (lambda (i) (program i)) parents))
         (new-expression (operate operacion (language algorithm) program-list)))
    (when (multiple-value-bind (x y)
              (parse (grammar (language algorithm)) new-expression)
            y)
      (progn nil))
    (prepare-children-from o new-expression algorithm)))