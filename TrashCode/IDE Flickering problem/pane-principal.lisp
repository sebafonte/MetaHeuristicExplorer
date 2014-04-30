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

(defmethod open-pane ((p pane-principal) &key mdi-interface)
  "Display <p> on <mdi-interface>."
  (declare (ignore mdi-interface))
  (capi:display (interface p)))


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
  (:default-initargs :title "Test"))

(defun reset-environment-callback (data interface)
  "Perform actions to reset system."
  (setf *tasks* nil)
  (reset-task-environment-settings)
  (mark-and-sweep 3))

(defun configure-callback (data interface)
  (prompt-for-plusp-integer "Configure"))

(defun exit-callback (data interface)
  (when (capi:confirm-yes-or-no "Exit environment")
    (capi:apply-in-pane-process interface 'capi:quit-interface interface)))

(defmethod maximize ((i interface-pane-principal))
  "Maximize <i>."
  (setf (capi:top-level-interface-display-state i) :maximized))
