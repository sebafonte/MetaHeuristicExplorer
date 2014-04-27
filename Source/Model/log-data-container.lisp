
(defclass log-data-container (base-model) 
  ((log-data :initarg :log-data :initform nil :accessor log-data)))


(defmethod add-value ((e log-data-container) log-data)
  "Register <log-data> on <e>."
  (push log-data (log-data e)))

(defmethod log-data-for-criteria ((e log-data-container) criteria)
  (select
   (log-data e)
   (lambda (a) (value-for a criteria))))

(defun save-log-data-set (object &rest args)
  "Save log-data descripted by <args> in <o>."
  (let ((arguments))
    (do ((i args (cddr i)))
        ((null i))
      (push (list (car i) (cadr i)) arguments))
    (add-value (log-data object) (make-instance 'log-data :data arguments))))



#|
(defmethod aggregated-value-for ((e log-data-container) reduce-operator &rest criteria-list)
  "Answer the value by <reduce-operator> for log data on <e> matching <criteria-list>."
  (let ((table (make-hash-table)))
    (dolist (data e)
      (setf (gethash table (criteria-values-list e))
            (append x (gethash table (criteria-values-list e)))))
    (maphash table (lambda (a) (reduce reduce-operator a)))))
|#
