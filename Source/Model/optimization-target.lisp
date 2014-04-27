
(defclass optimization-target (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)))


(defmethod initialize-properties :after ((o optimization-target))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type :default-value "Optimization target" 
    :data-type 'string :editor 'text-editor)))