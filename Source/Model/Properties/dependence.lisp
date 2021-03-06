
(defun has-dependency (a b)
  (equal (property (dependency a)) (name b)))

;; #TODO & #NOTE: Possible optimization hook
(defun has-dependency-to-update (p)
  (declare (ignore p))
  t)

(defmethod valid-dependency-value ((o t) property value)
  t)
  
  
(defclass property-dependence (base-model)
  ((property :initarg :property :accessor property)))

  
(defclass possible-class-dependence (property-dependence)
  ())

(defmethod valid-dependency-value ((o possible-class-dependence) property value)
  (not (null (find (class-of value) (mapcar 'class-of (possible-values property))))))

(defun make-possible-class-dependency (property)
  (make-instance 'possible-class-dependence :property property))


(defclass eql-dependence (property-dependence)
  ())

(defmethod valid-dependency-value ((o eql-dependence) property value)
  (not (null (find value (possible-values property)))))

(defun make-eql-dependence (property)
  (make-instance 'eql-dependence :property property))


(defclass eql-language-dependence (property-dependence)
  ())

(defmethod valid-dependency-value ((o eql-language-dependence) property value)
  (and value 
       (not (null (find-if 
                   (lambda (object) (compatible-language object value)) 
                   (possible-values property))))))

(defmethod compatible-language (a b)
  (eql (class-of a) (class-of b)))

(defun make-eql-language-dependence (property)
  (make-instance 'eql-language-dependence :property property))

