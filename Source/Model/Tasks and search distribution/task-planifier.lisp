(defparameter *task-planifier-lock* nil)
(defparameter *task-planifier-task-lock* (mp:make-lock :name "task-planifier-task-lock"))
(defparameter *auxiliar-lock* nil)
(defparameter *simultaneous-processes* 0)


(defclass task-planifier (object-with-properties)
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (local :initarg :local :accessor local)
   (remote :initarg :remote :accessor remote)
   (running-image :initarg :running-image :accessor running-image)
   (connection-administrator :initarg :connection-administrator :accessor connection-administrator)
   (max-simultaneous-processes :initarg :max-simultaneous-processes :accessor max-simultaneous-processes)))


(defmethod print-object ((o task-planifier) seq)
  (format seq "~A" (description o)))

(defmethod initialize-properties :after ((object task-planifier))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'string-editor :default-value "Task planifier")
   (:name 'description :label "Description" :accessor-type 'accessor-accessor-type 
    :data-type 'string  :editor 'text-editor)
   (:name 'connection-administrator :label "Connections administrator" :accessor-type 'accessor-accessor-type
    :data-type 'string :editor 'button-editor :default-value nil)
   (:name 'max-simultaneous-processes :label "Max simoultaneous processes" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'lisp-editor :default-value nil :object-parameter t)
   (:name 'local :label "Local hosts" :accessor-type 'accessor-accessor-type :editor 'boolean-editor :default-value t :data-type 'boolean)
   (:name 'remote :label "Remote hosts" :accessor-type 'accessor-accessor-type :editor 'boolean-editor :default-value t :data-type 'boolean)
   (:name 'running-image :label "Running image" :accessor-type 'accessor-accessor-type :editor 'boolean-editor :default-value t :data-type 'boolean)))

(defmethod active-connections ((o task-planifier))
  (let* ((local (local-active-connections (connection-administrator o)))
         (remote (remote-active-connections (connection-administrator o)))
         (running-image (list (system-get 'running-image-descriptor))))
    (reduce 'union 
            (list (if (local o) local nil)
                  (if (remote o) remote nil)
                  (if (running-image o) running-image nil)))))

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
        (signal-error-on planifier target task "Closed connection")))))

(defmethod execute-subtask-local ((planifier task-planifier) (subtask search-task))
  "Executes <subtask> on running image."
  (execute-subtask-loop subtask)
  subtask)

(defmethod execute-subtask-remote ((planifier task-planifier) (subtask search-task) (target connection-descriptor))
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
            (let ((initial-time (get-universal-time)))
              (setf (state subtask) 'RUNNING-REMOTE
                    (initial-time subtask) initial-time)
              (let* ((result (eval (read-from-string 
                                    (handler-case (read-line stream nil nil) 
                                      (error (error) 
                                        (signal-error-on planifier target subtask error)
                                        (incf (finished-tasks target))
                                        (setf (initial-time subtask) initial-time
                                              (final-time subtask) (get-universal-time))
                                        (return-from execute-subtask-remote subtask))))))
                     (content (content result)))
                (when (error-message-p result)
                  (signal-error-on planifier target subtask content))
                (update-transfered-subtask content subtask)
                (incf (finished-tasks target))
                (setf (initial-time content) initial-time
                      (final-time content) (get-universal-time))
                content)))
        (signal-error-on planifier target subtask "Closed connection")))))

(defmethod signal-error-on (planifier target task error)
  (setf (state task) 'error)
  ;(log-error *logger* error target task)
  ;(error (format nil "Error on <~a> with <~a>: ~a" target task error))
  )

(defun update-transfered-subtask (new-task task)
  "Update redundant information deleted to transfer <new-task>."
  (setf 
   ;; Set new task property definitions
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
  (mp:with-lock (*task-planifier-task-lock*)
    (let ((i (length (children task))))
      (loop as n = (decf i)
            while (>= n 0)
            do 
            (progn 
              (mp:process-wait "Waiting for process completion."
                             (lambda () 
                               (mp:with-lock (*task-planifier-lock*)
                                 (let ((value (max (minimum-simultaneous-processes task) 
                                                   (real-max-simultaneous-processes planifier))))
                                   (< *simultaneous-processes* value)))))
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
                     n)))))))

(defun minimum-processes ()
  (let ((tasks (remove nil *search-tasks*)))
    (when tasks
      (reduce 'min (mapcar 'minimum-simultaneous-processes tasks)))))

(defmethod minimum-simultaneous-processes ((o search-task))
  (minimum-simultaneous-processes-objetive o (objetive-class o)))

(defmethod minimum-simultaneous-processes-objetive ((o search-task) (class (eql 'search-task)))
  2)

(defmethod minimum-simultaneous-processes-objetive ((o search-task) class)
  1)
