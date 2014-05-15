(defparameter *task-planifier-lock* nil)
(defparameter *simultaneous-processes* 0)


(defclass task-planifier (object-with-properties)
  ((name :initarg :name :accessor name)
   (connection-administrator :initarg :connection-administrator :accessor connection-administrator)
   (max-simultaneous-processes :initarg :max-simultaneous-processes :accessor max-simultaneous-processes)))


(defmethod initialize-properties :after ((object task-planifier))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'string-editor :default-value "Task planifier")
   (:name 'connection-administrator :label "Connections administrator" :accessor-type 'accessor-accessor-type
    :data-type 'string :editor 'button-editor :default-value nil)
   (:name 'max-simultaneous-processes :label "Max simoultaneous processes" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :default-value 1 :min-value 1 :max-value 8 :object-parameter t)))

(defmethod copy ((object task-planifier))
  (let ((connection-administrator (connection-administrator object)))
    (setf (connection-administrator object) nil)
    (let ((new-instance (copy-instance object)))
      (setf (connection-administrator new-instance) connection-administrator
            (connection-administrator object) connection-administrator)
      new-instance)))

(defun simplified-copy (task)
  "Answer a simplified copy of <task>."
  (let ((task-copy (copy-cyclic task)))
    ;; Clean possible conflictive slots
    (setf (properties-definition task-copy) nil
          (properties-definition (algorithm task-copy)) nil
          (properties-definition (fitness-evaluator task-copy)) nil
          (properties-definition (selection-method (algorithm task-copy))) nil
          (properties-definition (initialization-method (algorithm task-copy))) nil)
    ;; Reset fitness evaluator temporary data
    (reset-temporary-data (fitness-evaluator task-copy))
    ;; Clean genetic operation property definitions
    (dolist (operation-probabilty (operators (algorithm task-copy)))
      (setf (properties-definition (car operation-probabilty)) nil))
    ;; Answer simplified copy
    task-copy))

(defmethod execute-task ((planifier task-planifier) task)
  "Executes a search-task using planifier to determine where to execute it.
   #NOTE: Execution is done into a mp:process-run-function with low priority."
  (setf (process task)
        (mp:process-run-function
         "Process task"
         (list :priority (priority task))
         (lambda ()
           (let ((result-task))
             (appendf *search-subtasks* (children task))
             (setf result-task (execute-task-local planifier task)))))))

(defmethod execute-task-local ((planifier task-planifier) (task search-task))
  "Executes <task> on <planifier>.
  #NOTE: Answer <task> because in remote execution instance a copy is returned"
  (execute-search task)
  task)

(defmethod execute-task-remote ((planifier task-planifier) (task search-task) (target connection-descriptor))
  "Executes <task> on a remote client application."
  (let ((message (make-tcp-message 'message-send-task task)))
    (with-open-stream
        (stream (comm:open-tcp-stream (ip-address target) (port target)))
      (if stream
          (progn
            (write-line (transportable-code-description message) stream)
            (force-output stream)
            (content (eval (read-from-string (read-line stream nil nil)))))
        ;; #TODO: Throw an error
        nil))))

(defmethod execute-subtask-local ((planifier task-planifier) (subtask search-task))
  "Executes <subtask> on running image."
  (execute-subtask-loop subtask)
  subtask)

(defmethod execute-subtask-remote ((planifier task-planifier) 
                                   (subtask search-task) 
                                   (target connection-descriptor))
  "Executes <subtask> on a remote image."
  (let* ((new-subtask (simplified-copy subtask))
         (message-string (make-tcp-message-string 'message-send-subtask new-subtask)))
    (with-open-stream
        (stream (comm:open-tcp-stream (ip-address target) (port target)))
      (if stream
          (progn
            (setf (state subtask) 'TRANSFERING)
            (write-line message-string stream)
            (force-output stream)
            (incf (tasks-asigned target))
            (setf (state subtask) 'RUNNING-REMOTE)
            (let ((initial-time (get-universal-time))
                  (result-subtask (content (eval (read-from-string (read-line stream nil nil))))))
              (update-transfered-subtask result-subtask subtask)
              (setf (initial-time result-subtask) initial-time
                    (final-time result-subtask) (get-universal-time))
              result-subtask))
        ;; #TODO: Throw an error
        nil))))

(defun update-transfered-subtask (new-task task)
  "Update redundant information deleted to transfer <new-task>."
  (setf ;; Set new task property definitions
        (properties-definition new-task) 
        (properties-definition task)
        ;; Set property definitions
        (properties-definition (algorithm new-task)) 
        (properties-definition (algorithm task))
        (properties-definition (fitness-evaluator new-task)) 
        (properties-definition (fitness-evaluator task))
        (properties-definition (selection-method (algorithm new-task))) 
        (properties-definition (selection-method (algorithm task)))
        (properties-definition (initialization-method (algorithm new-task)))
        (properties-definition (initialization-method (algorithm task)))
        ;; #TODO: Check if copy-cyclic is necessary
        ;; Connect new-task with algorithm
        (context (algorithm new-task)) new-task))

(defmethod select-task-target ((planifier task-planifier))
  "Answer a connection description to execute a task."
  (system-get 'running-image-descriptor))

(defmethod execute-task-subtasks ((planifier task-planifier) (task search-task))
  "Executes subtasks of <task> with <planifier>."
  (let ((i (length (children task))))
    (loop as n = (decf i)
          while (>= n 0)
          do 
          (progn 
            (mp:process-wait "Waiting for process completion."
                             (lambda () (< *simultaneous-processes* (real-max-simultaneous-processes planifier))))
            (mp:with-lock (*task-planifier-lock*)
              (incf *simultaneous-processes*))
            (setf (process task) 
                  (mp:process-run-function
                   "Process task"
                   (list :priority (priority task))
                   (lambda (children-number) 
                     (let ((result-task (execute-subtask
                                         planifier 
                                         (nth children-number (children task)))))
                       (mp:with-lock (*task-planifier-lock*)
                         (decf *simultaneous-processes*))
                       (setf (nth children-number (children task)) result-task)))
                   n))))))

(defmethod real-max-simultaneous-processes ((planifier task-planifier))
  (max-simultaneous-processes planifier))
