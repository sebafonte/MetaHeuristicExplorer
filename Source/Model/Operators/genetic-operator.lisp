(defclass genetic-operator (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)))


(defmethod initialize-properties :after ((object genetic-operator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string  :editor 'text-editor)))

(defmethod operate ((o genetic-operator) language expresions)
  (error "Subclass responsibility."))

(defmethod arity ((o genetic-operator))
  (error "Subclass responsibility."))


(defclass unary-genetic-operator (genetic-operator)
  ())

(defmethod operate ((o unary-genetic-operator) language expresions)
  (funcall (value-function o) (first expresions) language o))

(defmethod arity ((o genetic-operator))
  1)


(defclass binary-genetic-operator (genetic-operator)
  ())

(defmethod operate ((o binary-genetic-operator) language expresions)
  (funcall (value-function o) (first expresions) (second expresions) language o))

(defmethod arity ((o binary-genetic-operator))
  2)


