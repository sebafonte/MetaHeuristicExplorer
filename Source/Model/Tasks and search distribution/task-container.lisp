

(defclass task-container (base-model)
  ((name :initarg :name :accessor name)
   (elements :initarg :elements :initform nil :accessor elements)))


(defmethod set-elements ((o task-container) elements)
  (setf (elements o) elements))

(defmethod append-elements ((o task-container) elements)
  (appendf (elements o) elements))

(defmethod clear-elements ((o task-container))
  (setf (elements o) nil))