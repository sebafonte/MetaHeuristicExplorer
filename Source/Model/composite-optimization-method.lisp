
(defclass composite-optimization-method (optimization-method)
  ((method-list :initarg :method-list :accessor method-list)))


(defmethod initialize-properties :after ((a composite-optimization-method))
  "Initialize <a> properties."
  (add-properties-from-values
   a 
   (:name 'method-list :label "Methods" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :editor 'list-editor)
   (:name 'optimization-method :label "Method" :accessor-type 'valuable-accessor-type 
    :data-type 'object :default-value nil :editor 'list-editor :visible nil)))

(defmethod check-optimization ((o composite-optimization-method))
  (block 1
    (dolist (method (method-list o))
      (if (check-optimization method)
          (return-from 1 t)))
    nil))

(defmethod execute-optimization-on ((method composite-optimization-method) (o t))
  (dolist (m (method-list method))
    (if (check-optimization m)
        (execute-optimization-on m o))))

(defmethod add-method ((composite composite-optimization-method) (simple optimization-method))
  (appendf (method-list composite) simple))

