
(defclass log-inspector ()
  ((name :initarg :name :accessor name)
   (event :initarg :event :accessor event)
   (action :initarg :action :accessor action)
   (subject :initarg :subject :accessor subject)))


(defmethod apply-log-inspector-on ((o log-inspector))
  (when-send-to (subject o) (event o) (action o) o))