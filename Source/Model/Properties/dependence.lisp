
(defclass property-dependence (base-model)
  ((subject :initarg :subject :accessor :subject)))


(defclass possible-class-dependence (property-dependence)
  ())

(defmethod valid-dependency-value ((o possible-class-dependence) property value)
  (not (null (find (class-of value) (mapcar 'class-of (possible-values property))))))


(defclass eql-dependence (property-dependence)
  ())

(defmethod valid-dependency-value ((o eql-dependence) property value)
  (not (null (find value (possible-values property)))))

