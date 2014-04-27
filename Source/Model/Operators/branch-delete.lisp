(defclass branch-delete (genetic-operator)
  ((node-selection-function :initarg :node-selection-function :accessor node-selection-function)))


(defmethod initialize-properties :after ((object branch-delete))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'node-selection-function :label "Min size" :accessor-type 'accessor-accessor-type 
    :data-type 'list :min-value 1 :max-value 10000 :default-value 1 :editor 'function-editor)))

(defmethod arity ((o genetic-operator))
  1)

(defmethod operate ((o branch-delete) language expresions)
  (let* ((expresion (first expresions))
         (point-index (random (tree-size expresion)))
         (subtree (create-expresion language 1 1 t nil)))
    (replace-internal-subtree expresion subtree point-index language)))


