(defclass registrable-object-wrapper (base-model)
  ((name :initarg :name :accessor name)
   (subject :initarg :subject :accessor subject)
   (description :initarg :description :initform nil :accessor description)))


(defmethod initialize-instance :after ((r registrable-object-wrapper) &key subject name description)
  "Initialize <r>."
  (setf (slot-value r 'name) name
        (slot-value r 'subject) subject
        (slot-value r 'description) description)
  (system-add r))

(defmethod print-object ((r registrable-object-wrapper) seq)
  (format seq "~A" (if (description r) (description r) (name r))))

