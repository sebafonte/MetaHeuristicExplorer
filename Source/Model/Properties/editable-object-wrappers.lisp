
(defclass editable-object-wrapper (object-with-properties)
  ())

(defclass editable-probability-object-list-wrapper (editable-object-wrapper)
  ((subject :initarg :subject :initform nil :accessor subject)))


(defmethod initialize-instance :after ((o editable-probability-object-list-wrapper) &rest key)
  "Initialize <o>."
  (initialize-subject-properties o))

(defmethod initialize-subject-properties ((o editable-probability-object-list-wrapper))
  "Initialize <o> subject properties."
  (let ((name-symbol-index 0))
    (dolist (i (subject o))
      (incf name-symbol-index)
      (add-properties-from-values
       o
       (:name name-symbol-index :label (format nil "~A" (name (car i))) 
        :accessor-type 'property-accessor-type :data-type 'list :min-value 0 :max-value 1
        :editor 'editable-parameter-editor :subject i))
      (set-value-for-property-named o name-symbol-index (cadr i)))))

(defmethod (setf subject) (object (o editable-probability-object-list-wrapper))
  (setf (slot-value o 'subject) object)
  (initialize-subject-properties o))

(defmethod update ((o editable-probability-object-list-wrapper))
  "Update <o> wrapped object."
  (dolist (p (properties-definition o))
    (setf (cadr (subject p))
          (get-value-for-property o p))))
