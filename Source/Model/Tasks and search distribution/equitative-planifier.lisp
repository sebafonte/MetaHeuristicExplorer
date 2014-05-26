(defparameter *target-task-assignment* nil)


(defclass equitative-planifier (task-planifier)
  ())


(defmethod connections-by-load ((p equitative-planifier))
  (sort (active-connections p) 'connection-load-comparator))

(defun connection-load-comparator (a b)
  (< (connection-load a) (connection-load b)))

(defun connection-load (a)
  (current-tasks a))

(defmethod select-subtask-target ((planifier equitative-planifier) task)
  "Answer a connection description to execute <task>."
  (declare (ignore task))
  (let ((list (active-connections planifier)))
    (if (null list)
        (error "No available remote target")
      (first (connections-by-load planifier)))))

(defmethod execute-subtask ((planifier equitative-planifier) (task search-task))
  (let ((target))
    (mp:with-lock (*task-planifier-lock*)
      (let ((task-target (select-subtask-target planifier task)))
        (setf target task-target)))
    (if target
        (progn 
          (incf (tasks-asigned target))
          (if (is-running-image target)
              (execute-subtask-local planifier task)
            (execute-subtask-remote planifier task target)))
      (error "No available remote target"))))
