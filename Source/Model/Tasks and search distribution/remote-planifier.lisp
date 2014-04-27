
(defclass remote-planifier (task-planifier)
  ())


(defmethod select-subtask-target ((planifier remote-planifier) (subtask search-task))
  "Answer a connection description to execute a <subtask>."
  (declare (ignore subtask))
  (first (remote-active-connections (system-get 'main-connection-administrator))))

(defmethod execute-subtask ((planifier remote-planifier) (subtask search-task))
  (let ((target (select-subtask-target planifier subtask)))
    (execute-subtask-remote planifier subtask target)))


 
