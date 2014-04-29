(defvar *tasks* nil)


(defclass pane-principal (base-pane)
  ((panes :initform nil :accessor panes)))


(defmethod initialize-instance :after ((p pane-principal) &optional &key mdi-interface (open t))  
  "Initialize <p>."
  (declare (ignore mdi-interface))
  (maximize (interface p)))

(defmethod interface-class ((p pane-principal))
  "Answer <p> interface class."
  'interface-pane-principal)

(defun open-pane-tasks (interface data)
  "Open a 'pane-tasks on <interface>."
  (declare (ignore data))
  (make-instance 'pane-tasks :mdi-interface interface :tasks *tasks*))

(defun open-pane-listener (interface data)
  "Open a pane-listener on <interface>."
  (declare (ignore data) (ignore interface))
  (let ((listener-interface (make-instance 'capi:listener-pane))
        (screen (capi:screen-active-interface (capi:element-screen interface))))
    (capi:contain listener-interface :best-width 300 :best-height 200 :screen screen)
    (setf (capi:simple-pane-background listener-interface) (color:make-rgb 0 0 0)
          (capi:simple-pane-foreground listener-interface) (color:make-rgb 0 1 0))))

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
                                :selection-callback 'open-pane-tasks
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
    "File" (("Exit" :callback 'exit-callback))))
  (:menu-bar menu-principal)
  (:layouts (main-layout capi:column-layout '(simple-toolbar :divider capi:container) 
                         :ratios '(1 nil 4)))
  (:default-initargs :best-width 1280 :best-height 800 :title "Test"))

(defun reset-environment-callback (data interface)
  "Perform actions to reset system."
  ;; Delete all tasks
  (setf *tasks* nil)
  ;; Reset task planifier
  (reset-task-environment-settings)
  ;; Run GC
  (mark-and-sweep 3))

(defun configure-callback (data interface)
  (prompt-for-plusp-integer "Configure"))

(defun exit-callback (data interface)
  (when (capi:confirm-yes-or-no "Exit environment")
    (capi:apply-in-pane-process interface 'capi:quit-interface interface)))

(defmethod open-pane ((p pane-principal) &key mdi-interface)
  "Display <p> on <mdi-interface>."
  (declare (ignore mdi-interface))
  (capi:display (interface p)))

(defmethod maximize ((i interface-pane-principal))
  "Maximize <i>."
  (setf (capi:top-level-interface-display-state i) :maximized))
