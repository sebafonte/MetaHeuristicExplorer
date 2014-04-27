
(defclass task-builder (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)))


(defmethod initialize-properties :after ((o task-builder))
  "Initializes <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :default-value "Task builder" :editor 'text-editor)))

(defmethod abstractp ((o (eql 'task-builder)))
  "Answer whether <o> is abstract."
  t)
