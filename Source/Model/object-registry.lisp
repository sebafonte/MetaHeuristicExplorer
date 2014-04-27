(defclass system-registry (base-model)
  ((registry :initarg :registry :initform (make-hash-table) :accessor registry)))

(defvar *system-registry* (make-instance 'system-registry))


(defmethod get-object ((o system-registry) name)
  (gethash name (registry o)))
      
(defmethod add ((registry-object system-registry) object)
  (setf (gethash (name object) (registry registry-object)) object))

(defmethod add-with-name ((registry-object system-registry) object name)
  (setf (gethash name (registry registry-object)) object))

(defmethod system-add (&rest objects)
  (dolist (o objects)
    (add *system-registry* o)))

(defmethod system-add-with-name (object name)
  (add-with-name *system-registry* object name))

(defmethod system-get ((name t))
  (let ((result (get-object *system-registry* name)))
    (if result result
      (progn 
        (error "System object requested not found.")
        result))))

(defmethod system-get-copy ((name t))
  (copy (system-get name)))

(defmethod system-get-subject ((name t))
  (subject (system-get name)))

(defmethod system-get-subject-copy ((name t))
  (copy (subject (system-get name))))

(defmethod system-clear ()
  (setf *system-registry* (make-instance 'system-registry)))
