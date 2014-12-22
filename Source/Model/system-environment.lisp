
(defclass system-environment (base-model)
  ((tasks :initarg :tasks :accessor tasks)
   (subtasks :initarg :subtasks :accessor subtasks)
   (panes :initarg :panes :accessor panes)))


(defun dump-system-environment ()
  "Answer a new system-environment instance with system actual state."
  (make-instance 'system-environment
                 :tasks (elements *search-tasks*)
                 :subtasks (elements *search-subtasks*)
                 :panes (system-panes-description)))

(defmethod update-system ((e system-environment))
  "Update system for <e>."
  ;; Update global task list
  ;(append-elements *search-tasks* (tasks e))
  (set-elements *search-tasks* (tasks e))
  ;; Update active search subtasks
  ;(append-elements *search-subtasks* (subtasks e))
  (set-elements *search-subtasks* (subtasks e))
  ;; Update interface
  (dolist (p (panes e))
    (new-from-description p)))

(defun system-panes-description ()
  "Answer a list of pane descriptions for panes on GUI."
  (mapcar 
   (lambda (object) (get-description (pane object)))
   (capi:collect-interfaces 'base-interface :screen (interface *main-pane*))))
