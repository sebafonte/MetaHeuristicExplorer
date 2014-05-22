(defparameter *target-task-assignment* nil)


(defclass equitative-planifier (task-planifier)
  ((adjust-one-process-per-host :initarg :adjust-one-process-per-host :accessor adjust-one-process-per-host)))


(defmethod initialize-properties :after ((object equitative-planifier))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'adjust-one-process-per-host :label "Adjust one process per host" :accessor-type 'accessor-accessor-type
    :data-type :boolean :editor 'boolean-editor :default-value t)))

(defmethod connections-by-load ((p balanced-planifier))
  (sort (active-connections (connection-administrator p)) 
        'connection-load-comparator))

(defun connection-load-comparator (a b)
  (> (connection-load a)
     (connection-load b)))

(defmethod select-subtask-target ((planifier equitative-planifier) task)
  "Answer a connection description to execute <task>."
  (declare (ignore task))
  (first (connections-by-load planifier)))

(defmethod execute-subtask ((planifier equitative-planifier) (task search-task))
  (let* ((target (assign-subtask planifier task))
         (result (if (eql (system-get 'running-image-descriptor) target)
                     (execute-subtask-local planifier task)
                   (execute-subtask-remote planifier task target))))
    (signal-termination task target)
    result))
