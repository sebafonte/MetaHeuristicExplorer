(defparameter *target-task-assignment* nil)


(defclass equitative-planifier (task-planifier)
  ((include-running-image :initarg :include-running-image :accessor include-running-image)
   (adjust-one-process-per-host :initarg :adjust-one-process-per-host :accessor adjust-one-process-per-host)))


(defmethod initialize-properties :after ((object equitative-planifier))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'include-running-image :label "Include running image" :accessor-type 'accessor-accessor-type
    :data-type 'boolean :editor 'boolean-editor :default-value t)
   (:name 'adjust-one-process-per-host :label "Adjust one process per host" :accessor-type 'accessor-accessor-type
    :data-type :boolean :editor 'boolean-editor :default-value t)))

(defmethod update-connections ((planifier equitative-planifier))
  (let ((new-connections))
    (dolist (c (connections planifier))
      (let ((found (find c *target-task-assignment* 
                         :test (lambda (item object) (equal (car object) item)))))
        (if (is-connected c)
            (appendf new-connections 
                     (list (list c (if found (cadr found) 0)))))))
    (setf *target-task-assignment* new-connections)))

(defmethod connections ((planifier equitative-planifier))
  (active-connections (system-get 'main-connection-administrator)))

(defmethod select-subtask-target ((planifier equitative-planifier) subtask)
  "Answer a connection description to execute <subtask>."
  (declare (ignore subtask))
  (update-connections planifier)
  (first (sort *target-task-assignment* (lambda (x y) (< (cadr x) (cadr y))))))

(defmethod assign-subtask ((planifier equitative-planifier) subtask)
  "Assign <subtask> to a target and mark the assignment list, then answer the target."
  (declare (ignore task))
  (let* ((target (select-subtask-target planifier subtask))
         (target-tasks (find target *target-task-assignment*)))
    (mp:with-lock (*task-planifier-lock*)
      (if (null target-tasks)
          (appendf *target-task-assignment* (list (list target 1)))
        (setf (cadr target-tasks) (+ (cadr target-tasks) 1))))
    (first target)))

(defmethod execute-subtask ((planifier equitative-planifier) (subtask search-task))
  (let* ((target (assign-subtask planifier subtask))
         (result (if (eql (system-get 'running-image-descriptor) target)
                     (execute-subtask-local planifier subtask)
                   (execute-subtask-remote planifier subtask target))))
    (signal-termination subtask target)
    result))

(defmethod signal-termination (process target)
  (declare (ignore process))
  (let ((target-tasks (find target *target-task-assignment*
                            :test (lambda (item object) (equal (car object) item)))))
    (mp:with-lock (*task-planifier-lock*)
      (setf (cadr target-tasks) (- (cadr target-tasks) 1)))))

(defmethod real-max-simultaneous-processes ((p equitative-planifier))
  "Answer the real amount of processes that can be executed by <p>."
  (if (adjust-one-process-per-host p)
      (length (connections (connection-administrator p)))
    (max-simultaneous-processes p)))