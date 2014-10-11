
;;; Timer for client notification event
(defparameter *ip-update-timer* nil)
(defparameter *ip-update-timer-initial-delta* 10000)
(defparameter *ip-update-timer-delta* 300000)
(defparameter *tcp-default-timeout* 1)
(defparameter *tcp-long-timeout* 2)
(defparameter *main-server-address* "gpexplorer.ddns.net")


(defun initialize-main-server-informer-timer ()
  "Initializes the ip update timer of the system.
   #NOTE: This allows the main server to know about remote clients."
  (setf *ip-update-timer* (mp:make-timer (lambda () (safe-main-server-informer-callback))))
  (mp:schedule-timer-milliseconds *ip-update-timer* *ip-update-timer-initial-delta* *ip-update-timer-delta*))

(defun safe-main-server-informer-callback ()
  (mp:process-run-function "Refresh subtasks" nil
                           (lambda () (main-server-informer-callback))))

(defun ip-from-dns (string)
  "Answer the address 32bit number of string address in <string>."
  (comm:get-host-entry string :fields '(:address)))

(defun main-server-descriptor ()
  "Answer a new connection descriptor which points to main server."
  (let ((ip-address (ip-from-dns *main-server-address*)))
    (make-instance 'connection-descriptor
                   :ip-address ip-address
                   :port 20000
                   :descriptor-machine-instance "Genetic explorer main server"
                   :is-remote t)))

(defun main-server-informer-callback ()
  "Callback - Send messages to main server informing state of the running image (state, ip and port)."
  (let ((main-description (system-get 'running-image-descriptor))
        (descriptor (main-server-descriptor)))
    (with-open-stream       
        (stream (comm:open-tcp-stream (ip-address descriptor) (port descriptor) :timeout *tcp-default-timeout*))
      (if stream
          (let ((message (make-tcp-message-string 'message-remote-client-advisor
                                                  (list (comm:socket-stream-address stream)
                                                        (port main-description)
                                                        (machine-instance)))))
            (write-line message stream)
            (force-output stream)
            t)
        nil))))
