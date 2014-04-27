
(defclass log-data (base-model)
  ((data :initarg :data :initform nil :accessor data)))


(defmethod value-for ((o log-data) criteria)
  (cadr (assoc criteria (data o))))

(defun make-log-data ()
  (make-instance 'log-data))
