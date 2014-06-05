(defclass tcp-message (base-model)
  ((name :initarg :name :accessor name)
   (content :initarg :content :accessor content)))


(defmethod error-message-p ((o tcp-message)) 
  (eql (name o) 'message-error))

(defun initialize-default-tcp-messages ()
  (system-add
   ;; Connection hand-shaking / status messages
   (make-instance 'tcp-message :name 'message-open-connection :content "Genetic explorer 0.25 image.")
   (make-instance 'tcp-message :name 'message-close-connection :content nil)
   (make-instance 'tcp-message :name 'message-ping :content nil)
   (make-instance 'tcp-message :name 'message-pong :content nil)
   (make-instance 'tcp-message :name 'message-port-ping :content nil)
   ;; Basic signaling
   (make-instance 'tcp-message :name 'message-ok :content nil)
   (make-instance 'tcp-message :name 'message-error :content nil)
   ;; Task managament messages
   (make-instance 'tcp-message :name 'message-send-subtask :content nil)
   (make-instance 'tcp-message :name 'message-send-subtask-result :content nil)
   (make-instance 'tcp-message :name 'message-send-task :content nil)
   (make-instance 'tcp-message :name 'message-send-task-result :content nil)
   (make-instance 'tcp-message :name 'message-evaluate-object :content nil)
   ;; Statistics managament messages
   (make-instance 'tcp-message :name 'message-get-stats :content nil)
   (make-instance 'tcp-message :name 'message-clear-stats :content nil)
   ;; Performance test messages
   (make-instance 'tcp-message :name 'message-run-performance-test :content nil)
   (make-instance 'tcp-message :name 'message-tx-performance-test :content nil)
   (make-instance 'tcp-message :name 'message-rx-performance-test :content nil)
   ;; Object list / object sharing messages
   (make-instance 'tcp-message :name 'message-object-list-request :content nil)
   (make-instance 'tcp-message :name 'message-object-list-send :content nil)
   (make-instance 'tcp-message :name 'message-object-request :content nil)
   (make-instance 'tcp-message :name 'message-object-send :content nil)
   (make-instance 'tcp-message :name 'message-object-property-update :content nil)
   ;; Image purging / control
   (make-instance 'tcp-message :name 'message-respawn-image :content nil)
   (make-instance 'tcp-message :name 'message-clean-image :content nil)
   (make-instance 'tcp-message :name 'message-update-version :content nil)
   (make-instance 'tcp-message :name 'message-remote-client-advisor :content nil)))
   


;;; Behaviour dispatch: 
;;;   - Using message name not class at the moment (it´s enough at the moment)
;;;   - Administrator could be used in the body
;;;
(defmethod dispatch-message-name ((message-name t) message administrator stream)
  nil)

(defmethod dispatch-message-name ((message-name (eql 'message-object-request)) message administrator stream)
  nil)

(defmethod dispatch-message-name ((message-name (eql 'message-object-send)) message administrator stream)
  (open-in-new-editor (content message) (interface *main-pane*)))

(defmethod dispatch-message-name ((message-name (eql 'message-ping)) message administrator stream)
  (format stream (make-tcp-message-string 'message-pong nil))
  (force-output stream))

(defmethod dispatch-message-name ((message-name (eql 'message-run-performance-test)) message administrator stream)
  (format stream (transportable-code-description (timed-performance-test (content message))))
  (force-output stream))

(defmethod dispatch-message-name ((message-name (eql 'message-tx-performance-test)) message administrator stream)
  (format stream "ACK~%")
  (force-output stream))

(defmethod dispatch-message-name ((message-name (eql 'message-rx-performance-test)) message administrator stream)
  (format stream
          (make-tcp-message-string 
           'message-rx-performance-test 
           (create-sample-performance-test-file)))
  (force-output stream))

(defmethod dispatch-message-name ((message-name (eql 'message-send-task)) message administrator stream)
  (let ((task (content message)))
    (appendf *search-subtasks* (subtasks task))
    (execute-search task)
    (format stream (make-tcp-message-string 'message-send-task-result (simplified-copy task)))
    (force-output stream)))

(defmethod dispatch-message-name ((message-name (eql 'message-send-subtask)) message administrator stream)
  (let ((subtask (content message)))
    (appendf *search-subtasks* (list subtask))
    ;; #TODO: Add a "incoming tasks planifier" to accept all tasks or delegate on other locally known hosts
    (let ((error))
      (handler-case (execute-subtask-local (system-get 'global-running-image-planifier) subtask)
        (error (function) (setf error function) nil))
      (format stream 
              (if error
                  (make-tcp-message-string 'message-error (error-description error))
                (make-tcp-message-string 'message-send-subtask-result (simplified-copy subtask))))
      (force-output stream))))

(defun error-description (error)
  (format nil "~a" error))

(defmethod dispatch-message-name ((message-name (eql 'message-clean-image)) message administrator stream)
  "Executes garbage collector and purge all known buffers and unused object."
  (reset-environment-callback nil nil)
  ;(format stream (make-tcp-message-string 'message-ok))
  ;(force-output stream)
  )

(defmethod dispatch-message-name ((message-name (eql 'message-remote-client-advisor)) message administrator stream)
  "Updates client version automatically to current version if newer."
  (multiple-value-bind (address port)
      (comm:socket-stream-address stream)
    (let* ((content (content message))
           (address (nth 0 content))
           (port (nth 1 content))
           (machine-instance (nth 2 content)))
      (if (not (has-connection-with administrator address port))
          (add-connection administrator (make-instance 'connection-descriptor
                                                       :ip-address address
                                                       :port port
                                                       :state 'CONNECTED
                                                       :descriptor-machine-instance machine-instance
                                                       :is-remote t))))))

;; #TODO:
(defmethod dispatch-message-name ((message-name (eql 'message-update-version)) message administrator stream)
  "Updates client version automatically to current version if newer."
  nil)

(defmethod dispatch-message-name ((message-name (eql 'message-respawn-image)) message administrator stream)
  "Relaunch de current image to ensure the best client clean up."
  nil)

;;; Utility functions
(defun make-tcp-message-string (name content)
  (transportable-code-description (make-tcp-message name content)))

(defun make-tcp-message (name content)
  "Creates a new tcp message copy of system registered message <name> with <content>."
  (let ((new-message (copy (system-get name))))
    (setf (content new-message) content)
    new-message))

