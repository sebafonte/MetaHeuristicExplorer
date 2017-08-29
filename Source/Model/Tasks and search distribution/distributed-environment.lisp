(defclass distributed-environment (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)
   (connection-administrator :initarg :connection-administrator :accessor connection-administrator)
   (task-planifier-class :initarg :task-planifier-class :accessor task-planifier-class)
   (task-planifier :initarg :task-planifier :accessor task-planifier)))


(defmethod initialize-properties :after ((e distributed-environment))
  "Initialize <e> properties."
  (add-properties-from-values
   e
   (:name 'task-planifier-class :label "Task planifier class" :accessor-type 'accessor-accessor-type
    :data-type 'symbol :default-value (first (default-task-planifiers e)) 
    :possible-values (default-task-planifiers e) :editor 'list-editor 
    :update-callback 'lambda-reset-task-planifier-class :setter #'(setf task-planifier-class))
   (:name 'task-planifier :label "Task planifier" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor :default-value (make-instance 'task-planifier))
   (:name 'connection-administrator :label "Connections administrator" :accessor-type 'accessor-accessor-type
    :data-type 'object :editor 'button-editor :default-value (system-get 'main-connection-administrator))))

(defmethod initialize-instance :after ((e distributed-environment) &rest args)
  (declare (ignore args))
  "Initialize <e>."
  (setf (connection-administrator (task-planifier e)) (connection-administrator e)))

(defmethod default-task-planifiers ((e distributed-environment))
  (list 'task-planifier))

(defmethod add-connection ((environment distributed-environment) (descriptor connection-descriptor))
  "Add <descriptor> to <environment>."
  (add-connection (connection-administrator environment) descriptor))

(defmethod delete-connection ((environment distributed-environment) (descriptor connection-descriptor))
  "Deletes <descriptor> from <environment>."
  (delete-connection (connection-administrator environment) descriptor))

(defmethod benchmark-connection ((environment distributed-environment) (descriptor connection-descriptor))
  "Executes performance and integrity test on host image identified by <descriptor>."
  (benchmark descriptor))
  
(defmethod check-connections-state ((environment distributed-environment))
  "Check connection state for connections of <environment>."
  (check-connections-state (connection-administrator environment)))
