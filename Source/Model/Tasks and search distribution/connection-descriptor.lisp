(defclass connection-descriptor (object-with-properties)
  (;; Descriptors
   (name :initarg :name :initform nil :accessor name)
   (descriptor-machine-instance 
    :initarg :descriptor-machine-instance :initform nil :accessor descriptor-machine-instance)
   ;; Addresses
   (ip-address :initarg :ip-address :initform "localhost" :accessor ip-address)
   (port :initarg :port :accessor port)
   ;; State
   (state :initarg :state :initform 'disconnected :accessor state)
   (descriptor-stream :initarg :descriptor-stream :accessor descriptor-stream)
   (is-remote :initarg :is-remote :initform t :accessor is-remote)
   ;; Benchmark tracking
   (b-cpu :initarg :b-cpu :initform 0 :accessor b-cpu)
   (b-ping :initarg :b-ping :initform 0 :accessor b-ping)
   (b-rx :initarg :b-rx :initform 0 :accessor b-rx)
   (b-tx :initarg :b-tx :initform 0 :accessor b-tx)
   ;; Other
   (tasks-asigned :initarg :tasks-asigned :initform 0 :accessor tasks-asigned) 
   (finished-tasks :initarg :finished-tasks :initform 0 :accessor finished-tasks)))


(defmethod current-tasks ((o connection-descriptor))
  (- (tasks-asigned o) (finished-tasks o)))

(defmethod initialize-properties :after ((d connection-descriptor))
  "Initialize <d> properties."
  (add-properties-from-values
   d
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type
    :data-type 'symbol :default-value nil :editor 'symbol-editor)
   (:name 'ip-address :label "IP Address" :accessor-type 'accessor-accessor-type
    :data-type 'string :default-value "localhost" :editor 'text-editor)
   (:name 'port :label "Port" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :editor 'lisp-editor :default-value nil)
   (:name 'state :label "State" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'symbol-editor :default-value nil)
   (:name 'descriptor-machine-instance :label "Machine" :accessor-type 'accessor-accessor-type 
    :data-type 'string :editor 'text-editor :default-value "Default PC")
   (:name 'b-cpu :label "B.CPU" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :default-value 0 :read-only t)
   (:name 'b-ping :label "Ping" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :default-value 0 :read-only t)
   (:name 'b-tx :label "Tx" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :default-value 0 :read-only t)
   (:name 'b-rx :label "Rx" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :default-value 0 :read-only t)
   (:name 'is-remote :label "Remote" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :editor 'lisp-editor)
   (:name 'tasks-processing :label "Tasks processing" :accessor-type 'valuable-accessor-type 
    :getter '(lambda (descriptor)
               (length 
                (select *target-task-assignment* (lambda (object) (equal (car object) descriptor)))))
    :data-type 'integer :editor 'number-editor :visible t)
   (:name 'tasks-asigned :label "Tasks assigned" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :default-value 0)
   (:name 'finished-tasks :label "Finished assigned" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :default-value 0)
   (:name 'current-tasks :label "Current tasks" :accessor-type 'accessor-accessor-type :read-only t
    :data-type 'number :editor 'number-editor :default-value 0 :read-only t)))

(defmethod print-object ((o connection-descriptor) seq)
  (format 
   seq 
   (if (equal (state o) 'connected)
       "~A : ~A (ONLINE) - ~S" 
     "~A : ~A (OFFLINE) - ~S")
   (ip-address o) (port o) (name o)))

(defmethod send-object ((descriptor connection-descriptor) object)
  "Sends <object> through connection described by <descriptor>.
   Answer whether <object> has been sent."
  (with-open-stream
      (stream (comm:open-tcp-stream (ip-address descriptor) (port descriptor) :timeout *tcp-default-timeout*))
    (if stream
      (progn 
        (write-line (transportable-code-description object) stream)
        (force-output stream)
        t)
      nil)))

(defmethod check-state ((descriptor connection-descriptor))
  "Answer whether <descriptor> is still responding (like a simple ping for the application)."
  (with-open-stream
      (stream (comm:open-tcp-stream (ip-address descriptor) (port descriptor) :timeout *tcp-default-timeout*))
    (if stream
        (progn 
          (format stream (make-tcp-message-string 'message-port-ping nil))
          (force-output stream)
          (set-connected descriptor))
      (set-disconnected descriptor))))

(defmethod running-tasks ((descriptor connection-descriptor))
  "Answer a list with the tasks being executed at the moment on <descriptor>."
  nil)

(defmethod set-connected ((c connection-descriptor))
  "Set state of <c> to 'disconnected."
  (setf (state c) 'connected))

(defmethod set-disconnected ((c connection-descriptor))
  "Set state of <c> to 'connected."
  (setf (state c) 'disconnected))

(defmethod is-connected ((c connection-descriptor))
  "Answer whether <c> is connected."
  (eql (state c) 'connected))

(defmethod benchmark ((c connection-descriptor))
  "Executes basic system benchmarksof hosts pointed by <c>."
  ;; Perform PING benchmark
  (with-open-stream
      (stream (comm:open-tcp-stream (ip-address c) (port c) :timeout *tcp-default-timeout*))
    (when stream
      (let ((initial-time (get-internal-real-time)))
        (format stream (make-tcp-message-string 'message-ping nil))
        (force-output stream)
        (read-line stream nil nil)
        (setf (b-ping c) (- (get-internal-real-time) initial-time)))))
  ;; Perform CPU benchmark
  (with-open-stream
      (stream (comm:open-tcp-stream (ip-address c) (port c) :timeout *tcp-default-timeout*))
    (when stream
      (format stream (make-tcp-message-string 
                      'message-run-performance-test 
                      'performance-test-function-1))
      (force-output stream)
      (setf (b-cpu c) (eval (read-from-string (read-line stream nil nil))))))
  (let ((message-string)
        (initial-time))
    ;; Perform TX benchmark
    (with-open-stream
        (stream (comm:open-tcp-stream (ip-address c) (port c) :timeout *tcp-default-timeout*))
      (setf message-string (make-tcp-message-string 
                            'message-tx-performance-test 
                            (create-sample-performance-test-file)))
      (setf initial-time (get-internal-real-time))
      (format stream message-string)
      (force-output stream)
      (read-line stream nil nil) ; Wait for ACK
      (setf (b-tx c) 
            (round (coerce (* (/ 1000 1024)
                              (/ (length message-string) (- (get-internal-real-time) initial-time))) 'float))))
    ;; Perform RX benchmark
    (with-open-stream
        (stream (comm:open-tcp-stream (ip-address c) (port c) :timeout *tcp-default-timeout*))
      (setf initial-time (get-internal-real-time))
      (setf message-string (make-tcp-message-string 'message-rx-performance-test nil))
      (format stream message-string)
      (force-output stream)
      (setf initial-time (get-internal-real-time))
      (setf message-string (read-line stream nil nil))
      (setf (b-rx c)
            (round (coerce (* (/ 1000 1024)
                              (/ (length message-string) (- (get-internal-real-time) initial-time))) 'float))))))

(defmethod clear-status ((o connection-descriptor))
  (setf (tasks-asigned o) 0
        (finished-tasks o) 0))