
(defclass pane-principal (base-pane)
  ((panes :initform nil :accessor panes)))


(defmethod initialize-instance :after ((p pane-principal) &optional &key mdi-interface (open t))  
  "Initialize <p>."
  (declare (ignore mdi-interface open))
  (maximize (interface p)))

(defmethod interface-class ((p pane-principal))
  "Answer <p> interface class."
  'interface-pane-principal)

(defun open-pane-search-tasks (interface data)
  "Open a 'pane-search-tasks on <interface>."
  (declare (ignore data))
  (make-instance 'pane-search-tasks :mdi-interface interface :subtasks *search-tasks*))

(defun open-pane-editor-entity (interface data)
   "Open a 'pane-editor-entity on <interface>."
   (declare (ignore data))
   (make-editor-pane :mdi-interface interface))

(defun open-pane-explorer (interface data)
  "Open a 'pane-explorer on <interface>."
  (declare (ignore data))
  (make-instance 'pane-explorer :mdi-interface interface))

(defun open-pane-graphic (interface data)
  "Open a 'pane-graphic on <interface>."
  (declare (ignore data))
  (make-instance 'pane-graphic :mdi-interface interface))

(defun open-pane-buffer (interface data)
  "Open a 'pane-graphic on <interface>."
  (declare (ignore data))
  (make-instance 'pane-buffer :mdi-interface interface))

(defun open-pane-distributed-environment-configuration (interface data)
  "Open a 'pane-distribution-environment-configuration on <interface>."
  (declare (ignore data))
  (make-instance 'pane-distributed-environment-configuration 
                 :mdi-interface interface
                 :model (system-get 'local-distributed-environment)))

(defun open-pane-map (interface data)
  "Open a pane-map on <interface>."
  (declare (ignore data))
  (make-instance 'pane-map :mdi-interface interface))

(defun open-pane-listener (interface data)
  "Open a pane-listener on <interface>."
  (declare (ignore data))
  (let ((listener-interface (make-instance 'capi:listener-pane))
        (screen (capi:screen-active-interface (capi:element-screen interface))))
    (capi:contain listener-interface :best-width 300 :best-height 200 :screen screen)
    (setf (capi:simple-pane-background listener-interface) (color:make-rgb 0 0 0)
          (capi:simple-pane-foreground listener-interface) (color:make-rgb 0 1 0))))

(defun open-pane-test-runner (interface data)
  "Open a 'pane-test-runner on <interface>."
  (declare (ignore data))
  (open-pane (make-instance 'pane-test-runner :mdi-interface interface)))

(defun open-pane-performance-test-runner (interface data)
  "Open a 'pane-test-runner on <interface>."
  (declare (ignore data))
  (open-pane (make-instance 'pane-performance-test-runner :mdi-interface interface)))

(defun open-pane-settings (interface data)
  "Open a 'pane-settings on <interface>."
  (declare (ignore data))
  (open-in-new-editor (system-get 'system-configuration) interface))

(capi:define-interface interface-pane-principal (capi:document-frame base-interface)
  ((string :initarg :string))
  (:panes
   (simple-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button 
                                :image 10 
                                :selected-image 10
                                :tooltip "Tasks"
                                :help-key "Tasks"
                                :selection-callback 'open-pane-search-tasks
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 30 :selected-image 30
                                :tooltip "Editor"
                                :help-key "Editor"
                                :selection-callback 'open-pane-editor-entity
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 35 
                                :selected-image 35
                                :tooltip "Crossover editor"
                                :help-key "Crossover editor"
                                :selection-callback 'open-pane-explorer
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 32 
                                :selected-image 32
                                :tooltip "Objects map"
                                :help-key "Objects map"
                                :selection-callback 'open-pane-map
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 29 
                                :selected-image 29
                                :tooltip "Graphic editor"
                                :help-key "Graphic editor"
                                :selection-callback 'open-pane-graphic
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 33 
                                :tooltip "Buffer"
                                :selected-image 33
                                :help-key "Buffer"
                                :selection-callback 'open-pane-buffer
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 12 
                                :selected-image 12
                                :tooltip "Distributed environment"
                                :help-key "Distributed environment"
                                :selection-callback 'open-pane-distributed-environment-configuration
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 28 
                                :selected-image 28
                                :tooltip "Listener"
                                :help-key "Listener"
                                :selection-callback 'open-pane-listener
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 31 :selected-image 31
                                :tooltip "Settings"
                                :help-key "Settings"
                                :selection-callback 'open-pane-settings
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 34 
                                :selected-image 34
                                :tooltip "System tests"
                                :help-key "System tests"
                                :selection-callback 'open-pane-test-runner
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 36 
                                :selected-image 36
                                :tooltip "System performance tests"
                                :help-key "System performance tests"
                                :selection-callback 'open-pane-performance-test-runner
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button 
                                :image 37 
                                :selected-image 37
                                :tooltip "Command line scripting"
                                :help-key "Command line scripting"
                                :selection-callback 'open-pane-command-line
                                :callback-type :interface-data))
           :selection nil))
    :image-width 32
    :image-height 32
    :callback-type :interface
    :default-image-set (capi:make-general-image-set
                        :id 'global-button-icons-images-32
                        :image-count 37)))
  (:menus
   (menu-principal 
    "File" (("Configure" :callback 'configure-callback)
            ("Load environment" :callback 'load-environment-callback)
            ("Save environment" :callback 'save-environment-callback)
            ("Reset environment" :callback 'reset-environment-callback)
            ("Reset network" :callback 'reset-network-environment-callback)
            ("Reset interface bugs" :callback 'reset-interface-bugs-callback)
            ("Disable animation" :callback 'disable-animation-callback)
            ("Execute command" :callback 'execute-command-callback)
            ("Exit" :callback 'exit-callback))))
  (:menu-bar menu-principal)
  (:layouts (main-layout capi:column-layout '(simple-toolbar :divider capi:container) 
                         :ratios '(1 nil 4)))
  (:default-initargs :best-width 1280 :best-height 800 :title "v0.25"))


(defun load-environment-callback (data interface)
  (declare (ignore data interface))
  (let ((path (capi:prompt-for-file "Select environment to load"
                                    :filter "*.environment"
                                    :operation :open
                                    :filters '("Environment file" "*.environment"))))
    (when path
      (let ((environment (load-object-from path)))
        (update-system environment)))))

(defun save-environment-callback (data interface)
  (declare (ignore data interface))
  (let ((path (capi:prompt-for-file "Select environment to save"
                                    :filter "*.environment"
                                    :operation :save
                                    :filters '("Environment file" "*.environment"))))
    (when path
      (let ((environment (dump-system-environment)))
        (save-source-description environment path)))))

(defun reset-environment-callback (data interface)
  (declare (ignore data interface))
  "Perform actions to reset system."
  ;; Delete all tasks
  (setf *search-tasks* nil
        *search-subtasks* nil)
  ;; Reset task planifier
  (reset-task-environment-settings)
  ;; #TODO: Ver: Again?!
  (reset-task-environment-settings)
  ;; Run GC
  (mark-and-sweep 3))

(defun reset-network-environment-callback (data interface)
  (declare (ignore data interface))
  (mp:process-run-function "Resetting tasks"
                           '()
                           (lambda (administrator)
                             (dolist (i (active-connections administrator))
                               (reset-host-connection i)))
                           (system-get 'main-connection-administrator)))

(defun reset-host-connection (c)
  (with-open-stream       
      (stream (comm:open-tcp-stream (ip-address c) (port c)))
    (when stream
      (format stream (make-tcp-message-string 'message-clean-image nil))
      (force-output stream))))

(defun disable-animation-callback (data interface)
  (declare (ignore data interface))
  "Disables system animation. Also, set the animation frame counter to 1."
  ;; Reset mouse pointer icon
  (toggle-mouse-cursor (interface *main-pane*) nil)
  ;; Refresh screen editor panes
  (refresh-editors *visualization-mode*)
  ;; Run GC
  (mark-and-sweep 3)
  ;; Restart interface refresh processes
  (kill-updater)
  ;; Reset animations time counter variable
  (setf *time-variable* 1))

(defun reset-interface-bugs-callback (data interface)
  (declare (ignore data interface))
  "Perform actions to reset system."
  ;; Reset mouse pointer
  (toggle-mouse-cursor (interface *main-pane*) nil)
  ;; Refresh screen editor panes
  (refresh-editors *visualization-mode*)
  ;; Run GC
  (mark-and-sweep 3)
  ;; Restart interface refresh processes
  (kill-updater)
  (start-updater))

(defun execute-command-callback (data interface)
  (declare (ignore data interface))
  (let ((path (capi:prompt-for-file "Select command to execute"
                                    :filter "*.command"
                                    :operation :open
                                    :filters '("Command file" "*.command"))))
    (when path
      (apply-command (system-get 'main-command-line-interpreter) (list nil ":command-file" path)))))

(defun configure-callback (data interface)
  (declare (ignore data interface))
  (prompt-for-plusp-integer "Configure"))

(defun exit-callback (data interface)
  (declare (ignore data))
  (when (capi:confirm-yes-or-no "Exit environment")
    (capi:apply-in-pane-process interface 'capi:quit-interface interface)))

(defmethod open-pane ((p pane-principal) &key mdi-interface)
  "Display <p> on <mdi-interface>."
  (declare (ignore mdi-interface))
  (capi:display (interface p)))

(defmethod maximize ((i interface-pane-principal))
  "Maximize <i>."
  (setf (capi:top-level-interface-display-state i) :maximized))
