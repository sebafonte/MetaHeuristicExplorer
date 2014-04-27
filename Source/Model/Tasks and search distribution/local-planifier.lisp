
(defclass local-planifier (task-planifier)
  ())


(defmethod select-subtask-target ((planifier local-planifier) (subtask search-task))
  "Answer a connection description to execute <subtask>."
  (declare (ignore subtask))
  (first (select
          (local-active-connections (system-get 'main-connection-administrator)) 
          (lambda (o) (not (equal o (system-get 'running-image-descriptor)))))))

(defmethod execute-subtask ((planifier local-planifier) (subtask search-task))
  (let ((target (select-subtask-target planifier subtask)))
    (execute-subtask-remote planifier subtask target)))
 
