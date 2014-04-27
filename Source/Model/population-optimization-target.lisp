
(defclass population-optimization-target (optimization-target)
  ())


(defmethod target ((o population-optimization-target) (strategy optimization-strategy))
  (mapcar (lambda (object) 
            (make-instance 'object-in-search
                           :context (context (subject strategy))
                           :object object))
          (all-individuals (subject strategy))))
