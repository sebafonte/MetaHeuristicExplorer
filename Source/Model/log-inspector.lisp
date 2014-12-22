
(defclass log-inspector (base-model)
  ((name :initarg :name :accessor name)
   (event :initarg :event :accessor event)
   (action :initarg :action :accessor action)
   (subject :initarg :subject :accessor subject)))


(defmethod apply-log-inspector-on ((o log-inspector))
  (when-send-to (subject o) (event o) (action o) o))

(defmethod excluded-slot-name ((o log-inspector) slot-name)
  "Answer <o> slot names which are going to be excluded from source description."
  (or 
   (equal slot-name 'action)))
