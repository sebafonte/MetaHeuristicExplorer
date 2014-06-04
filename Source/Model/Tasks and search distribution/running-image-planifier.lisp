
(defclass running-image-planifier (task-planifier)
  ())


(defmethod initialize-properties :after ((object task-planifier))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'local :label "Local hosts" :accessor-type 'accessor-accessor-type :editor 'boolean-editor :default-value t :data-type 'boolean :visible nil)
   (:name 'remote :label "Remote hosts" :accessor-type 'accessor-accessor-type :editor 'boolean-editor :default-value t :data-type 'boolean :visible nil)
   (:name 'running-image :label "Running image" :accessor-type 'accessor-accessor-type :editor 'boolean-editor :default-value t :data-type 'boolean :visible nil)))

;; #TODO: Refactor with #'execute-task
(defmethod select-subtask-target ((planifier running-image-planifier) (subtask search-task))
  "Answer a connection description to execute a <subtask>."
  (declare (ignore subtask))
  (system-get 'running-image-descriptor))

(defmethod execute-subtask ((planifier running-image-planifier) (subtask search-task))
  (incf (tasks-asigned (system-get 'running-image-descriptor)))
  (execute-subtask-local planifier subtask))

(defmethod real-max-simultaneous-processes ((planifier running-image-planifier))
  (or (max-simultaneous-processes planifier)
      1))
