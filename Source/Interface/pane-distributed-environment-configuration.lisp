(defclass pane-distributed-environment-configuration (base-pane)
  ((model :initarg :model :accessor model)
   ;; #TODO: Replace this, enough for the moment but now sucks
   (timer :initform nil :accessor timer)))


(defmethod initialize-interface :after ((p pane-distributed-environment-configuration))
  (update-interface (interface p))
  (initialize-timer p))

(defmethod initialize-timer ((p pane-distributed-environment-configuration))
  "Initialize <p> timer."
  (setf (timer p) (mp:make-timer (lambda () (safe-refresh-connections p))))
  (mp:schedule-timer-milliseconds (timer p) 100 400))

(defmethod destroy ((p pane-distributed-environment-configuration))
  "Perform actions when destroying <p>."
  (mp:unschedule-timer (timer p)))

(defmethod safe-refresh-connections ((p pane-distributed-environment-configuration))
  "Refresh <p> connections in another mp:process."
  (mp:process-run-function 
   "Refresh connections" nil
   (lambda () (refresh-connections p))))

(defmethod refresh-connections ((p pane-distributed-environment-configuration))
  "Callback to refresh <p> connection."
  (when (interface p)
    (let ((connections (connections (interface p))))
      (capi:execute-with-interface
       (interface p)
       (lambda (&rest args) 
         (declare (ignore args))
         (let ((old-selection (capi:choice-selected-item connections)))
           (update-interface (interface p))
           (setf (capi:choice-selected-item connections) old-selection)))
       connections))))


(capi:define-interface interface-pane-distributed-environment-configuration (base-interface)
  ((sortable-lp-reverse :initform nil)
   (property-column-list :initform nil :accessor property-column-list))
  (:panes
   (label-connections capi:title-pane 
                     :text "Connections")
   (connections capi:multi-column-list-panel
                :items nil
                :action-callback 'open-properties-callback 
                :visible-min-height 70
                :accessor connections
                :header-args (list :print-function 'string-capitalize
                                   :selection-callback
                                   #'(lambda (interface new-sort-key)
                                       (declare (ignore interface))
                                       (set-multi-column-items
                                        (make-instance 'connection-descriptor)
                                        connections
                                        new-sort-key
                                        (if (eq sortable-lp-reverse new-sort-key)
                                            (progn
                                              (setf sortable-lp-reverse nil)
                                              t)
                                          (progn
                                            (setf sortable-lp-reverse new-sort-key)
                                            nil)))))
                :keep-selection-p t
                :pane-menu (lambda (pane object x y) 
                             (make-pane-menu 
                              pane 
                              object 
                              x 
                              y 
                              (options-menu-description-connection-descriptors))))
   (simple-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 2 :selected-image 2
                                :help-key "Add connection"
                                :selection-callback 'add-new-user-connection
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 1 :selected-image 1
                                :help-key "Delete connection"
                                :selection-callback 'delete-selected-connection-callback)
                 (make-instance 'capi:toolbar-button :image 5 :selected-image 5
                                :help-key "Connect"
                                :selection-callback 'scan-for-connections-callback)
                 (make-instance 'capi:toolbar-button :image 6 :selected-image 6
                                :help-key "Disconnect"
                                :selection-callback 'disconnect-connections-callback)
                 (make-instance 'capi:toolbar-button :image 7 :selected-image 7
                                :help-key "Scan connections"
                                :selection-callback 'scan-for-connections-callback)
                 (make-instance 'capi:toolbar-button :image 0 :selected-image 0
                                :help-key "Benchmark"
                                :selection-callback 'benchmark-connections-callback)
                 (make-instance 'capi:toolbar-button :image 3 :selected-image 3
                                :help-key "Load defaults"
                                :selection-callback 'restore-defaults-callback)
                 (make-instance 'capi:toolbar-button :image 4 :selected-image 4
                                :help-key "Save defaults"
                                :selection-callback 'save-defaults-callback)
                 (make-instance 'capi:toolbar-button :image 8 :selected-image 8
                                :help-key "Configure planifier"
                                :selection-callback 'configure-task-planifier))
           :selection nil))
    :callback-type :data-interface
    :title ""
    :title-position :frame
    :default-image-set (capi:make-general-image-set :id 'global-button-icons-images :image-count 8)))
  (:layouts
   (main-layout capi:column-layout '(label-connections connections simple-toolbar))
   (buttons 
    capi:row-layout '(button-add button-add button-disconnect button-delete button-scan button-test)))
  (:default-initargs :best-width 510 :best-height 230 :title "Distributed environment"))


(defun options-menu-description-connection-descriptors ()
  "Answer menu description for connection descriptors."
  '(("Check connection" check-selected-connection-callback)
    ("Benchmark connection" benchmark-selected-connection-callback)
    ("See stats" see-stats-callback)
    ("")
    ("Send buffered object" send-buffered-object-callback)))

;; #TODO: 
(defun see-stats-callback (interface data)
  (declare (ignore interface data))
  nil)

(defmethod default-connection-property-names ((i interface-pane-distributed-environment-configuration))
  "Answer a list with default property for connections to be shown on <i>."
  '(ip-address port descriptor-machine-instance state b-cpu b-ping b-rx b-tx current-tasks))

(defmethod initialize-instance :after ((i interface-pane-distributed-environment-configuration) &key)
  ;; Set interface properties
  (setf (property-column-list i) (default-connection-property-names i))
  ;; Set default columns
  (with-slots (connections) i
    (set-multi-column-items 
     (make-instance 'connection-descriptor)
     connections 
    "IP Address"))
  ;; Set column names function
  (setf (capi::multi-column-list-panel-column-function (connections i))
        (lambda (descriptor)
          (get-task-info descriptor i)))
  ;; Set interface columns
  (setf (capi::list-panel-columns (connections i))
        (columns-description i)))

(defmethod columns-description ((interface interface-pane-distributed-environment-configuration))
  "Answer the columns descriptions of interface."
  (loop for i in (property-column-list interface)
        collect 
        (let ((property (property-named (make-instance 'connection-descriptor) i)))
          (list :title (label property)
                :adjust (if (equal 'number (data-type property)) :right :left)
                :visible-min-width (list 'character (default-length-for-title interface (label property)))))))

(defmethod default-length-for-title ((interface interface-pane-distributed-environment-configuration) title)
  "Answer the default lenght for the title of interface."
  (let* ((default-lengths '(("IP address" . 15)
                            ("State" . 7)
                            ("Port" . 6)
                            ("B.CPU" . 7)
                            ("Ping" . 6)
                            ("Tx" . 4)
                            ("Rx" . 4)))
         (value (assoc title default-lengths :test 'equal)))
    (if value (cdr value)
      15)))

(defmethod update-interface ((i interface-pane-distributed-environment-configuration))
  "Update <i>."
  (setf (capi:collection-items (connections i))
        (connections (connection-administrator (model (pane i))))))

(defmethod interface-class ((p pane-distributed-environment-configuration))
  "Answer <p> interface class."
  'interface-pane-distributed-environment-configuration)

(defun add-new-user-connection (interface data)
  "Add a new connection configured by the user to the model of <p>."
  (declare (ignore data))
  (let* ((p (pane interface))
         (new-connection (create-new-user-connection p)))
    (when new-connection
      (add-connection (model p) new-connection)
      (update-interface interface))))

(defun delete-selected-connection-callback (data interface)
  (declare (ignore data))
  (delete-selected-connection 
   (pane interface)
   (capi:choice-selected-item (connections interface)))
  (update-interface interface))

(defun benchmark-selected-connection-callback (interface data)
  (declare (ignore data))
  (benchmark-selected-connection 
   (pane interface)
   (capi:choice-selected-item (connections interface)))
  (update-interface interface))

(defun send-buffered-object-callback (interface data)
  (declare (ignore data))
  (send-buffered-object
   (pane interface)
   (capi:choice-selected-item (connections interface))))  

(defun benchmark-connections-callback (data interface)
  (declare (ignore data))
  (benchmark-connections (pane interface))
  (update-interface interface))

(defun check-selected-connection-callback (interface data)
  (declare (ignore data))
  (check-connection-state (pane interface) (capi:choice-selected-item (connections interface))))

(defmethod check-connection-state ((p pane-distributed-environment-configuration) selected-connection)
  (declare (ignore p))
  (check-state selected-connection))

(defmethod benchmark-selected-connection ((p pane-distributed-environment-configuration) selected-connection)
  (declare (ignore p))
  (check-state selected-connection))

(defmethod send-buffered-object ((p pane-distributed-environment-configuration) selected-connection)
  (declare (ignore p))
  (let* ((buffered-object (copy (get-from-context *drag-context* :object)))
         (message (make-tcp-message 'message-object-send buffered-object)))
    (when buffered-object
      (send-object selected-connection message))))

(defmethod configure-task-planifier (data interface)
  (declare (ignore data))
  (open-editor-with interface (system-get 'local-distributed-environment)))

(defun scan-for-connections-callback (data interface)
  (declare (ignore data))
  (mp:process-run-function 
   "Connections scanner."
   nil
   (lambda () (check-connections-state (model (pane interface))))))

(defun save-defaults-callback (data interface)
  (declare (ignore data))
  (save-default-remote-connections (connection-administrator (model (pane interface)))))

(defun restore-defaults-callback (data interface)
  (declare (ignore data))
  (restore-default-remote-connections (connection-administrator (model (pane interface))))
  (update-interface interface))

(defmethod create-new-user-connection ((p pane-distributed-environment-configuration))
  (make-instance 'connection-descriptor 
                 :ip-address "200.0.0.1" 
                 :port 20000
                 :is-remote t))

(defmethod delete-selected-connection ((p pane-distributed-environment-configuration) selected-connection)
  "Remove all connection information from <p>."
  (if selected-connection
      (delete-connection (model p) selected-connection)))

(defmethod benchmark-connections ((p pane-distributed-environment-configuration))
  (dolist (c (active-connections (connection-administrator (model p))))
    (benchmark-selected-connection p c)))

(defmethod benchmark-selected-connection ((p pane-distributed-environment-configuration) selected-connection)
  "Execute speed tests for selected connection on <p>."
  (mp:process-run-function 
   "Process remote task"
   nil
   (lambda () 
     (if (and selected-connection (is-connected selected-connection))
         (benchmark-connection (model p) selected-connection)))))

(defmethod scan-for-connections ((p pane-distributed-environment-configuration))
  "Refresh neighbor information on the <p>."
  (mp:process-run-function 
   "Process remote task"
   nil
   (lambda () (scan-neighbor (model p)))))

(defmethod connect-selected-connection ((p pane-distributed-environment-configuration))
  "Connects <p> selected connection descriptor."
  (connect-to (model p) (capi:choice-selection (connections p))))

(defmethod disconnect-selected-connection ((p pane-distributed-environment-configuration))
  "Disconnects <p> selected connection descriptor."
  (disconnect-from (model p) (capi:choice-selection (connections p))))

(defmethod open-properties-callback (data interface)
  "Open a new pane-editor with <interface> selection as model."
  (open-in-new-editor 
   (capi:choice-selected-item (connections interface))
   interface))

(defun disconnect-connections-callback (data interface)
  (declare (ignore data))
  (disconnect-all (pane interface)))

(defmethod disconnect-all ((p pane-distributed-environment-configuration))
  (disconnect-all (connection-administrator (model p))))
