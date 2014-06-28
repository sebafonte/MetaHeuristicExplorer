
(defclass object-pool ()
  ((registry :initform (make-hash-table) :accessor registry)))


(defmethod clear ((o object-pool))
  (clear-hash (registry o)))

(defmethod get-object ((o object-pool) key)
  (get-hash (registry o) key))

(defmethod set-object ((o object-pool) key value)
  (setf (get-hash (registry o) key) value))

(defmacro check-object ((o object-pool) key &rest body)
  `(let ((value (get-object key)))
     (unless value
       (setf value (progn ,@body))
       (set-object o key value))
     value))

