
(defclass system-environment (base-model)
  ((tasks :initarg :tasks :accessor tasks)
   (subtasks :initarg :subtasks :accessor subtasks)
   (panes :initarg :panes :accessor panes)))


(defun dump-system-environment ()
  "Answer a new system-environment instance with system actual state."
  (make-instance 'system-environment
                 :tasks *search-tasks*
                 :subtasks *search-subtasks*
                 :panes (system-panes-description)))

(defmethod update-system ((e system-environment))
  "Update system for <e>."
  ;; Update global task list
  ;(appendf *search-tasks* (cdr (tasks e)))
  (setf *search-tasks* (tasks e))
  ;; Update active search subtasks
  ;(appendf *search-subtasks* (cdr (subtasks e)))
  (setf *search-subtasks* (cdr (subtasks e)))
  ;; Update interface
  (dolist (p (panes e))
    (new-from-description p)))

(defmethod system-panes-description ()
  "Answer a list of pane descriptions for panes on GUI."
  (mapcar 
   (lambda (object) (get-description (pane object)))
   (capi:collect-interfaces 'base-interface :screen (interface *main-pane*))))
