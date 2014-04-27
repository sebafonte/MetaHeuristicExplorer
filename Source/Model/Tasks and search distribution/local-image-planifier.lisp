(defclass running-image-planifier (task-planifier)
  ())


(defmethod select-task-target ((planifier local-image-planifier))
  "Answer a connection description to execute a task."
  (system-get 'running-image-descriptor))

