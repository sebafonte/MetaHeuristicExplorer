
(defclass random-task-planifier (task-planifier)
  ())

  
(defmethod select-subtask-target ((planifier random-task-planifier) (subtask search-task))
  "Answer a connection description to execute a <subtask>."
  (declare (ignore subtask))
  (let* ((local (local-active-connections (connection-administrator planifier)))
         (remote (remote-active-connections (connection-administrator planifier)))
         (running-image (list (system-get 'running-image-descriptor)))
         (connections (reduce 'union 
                              (list (if (local planifier) local nil)
                                    (if (remote planifier) remote nil)
                                    (if (running-image planifier) running-image nil)))))
    (random-element connections)))

(defmethod execute-subtask ((planifier random-task-planifier) (subtask search-task))
  (let ((target (select-subtask-target planifier subtask)))
    (if target
        (if (is-running-image target)
            (execute-subtask-local planifier subtask)
          (execute-subtask-remote planifier subtask target))
      (error "No available remote target"))))

(defun is-running-image (target)
  (equals target (system-get 'running-image-descriptor)))