
(defclass running-image-planifier (task-planifier)
  ())


;; #TODO: Refactor with #'execute-task
(defmethod select-subtask-target ((planifier running-image-planifier) (subtask search-task))
  "Answer a connection description to execute a <subtask>."
  (declare (ignore subtask))
  (system-get 'running-image-descriptor))

(defmethod execute-subtask ((planifier running-image-planifier) (subtask search-task))
  (incf (tasks-asigned (system-get 'running-image-descriptor)))
  (execute-subtask-local planifier subtask))
 