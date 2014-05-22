
(defclass random-task-planifier (task-planifier)
  ())


(defmethod select-subtask-target ((planifier random-task-planifier) (subtask search-task))
  "Answer a connection description to execute a <subtask>."
  (declare (ignore subtask))
  (random-element (active-connections planifier)))

(defmethod execute-subtask ((planifier random-task-planifier) (subtask search-task))
  (let ((target (select-subtask-target planifier subtask)))
    (if target
        (progn 
          (incf (tasks-asigned target))
          (if (is-running-image target)
              (execute-subtask-local planifier subtask)
            (execute-subtask-remote planifier subtask target)))
      (error "No available remote target"))))

(defun is-running-image (target)
  (equals target (system-get 'running-image-descriptor)))