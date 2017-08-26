(defparameter *visualization-mode* nil)
(defparameter *possible-visualization-modes nil)

(setf *possible-visualization-modes* (list (make-instance 'native-capi) (make-instance 'opengl-with-capi))
      *visualization-mode* (second *possible-visualization-modes*))


(defclass system-configuration (object-with-properties)
  ((name :initarg :name :accessor name)
   ;; Network
   (compact-tcp-messages :initarg :compact-tcp-messages :accessor compact-tcp-messages)
   (main-server :initarg :main-server :accessor main-server)
   ;; GUI
   (prompt-for-task-name :initarg :prompt-for-task-name :accessor prompt-for-task-name)
   (animation-enabled :initarg :animation-enabled :accessor animation-enabled)
   (panes-history-level :initarg :panes-history-level :accessor panes-history-level)
   (visualization-mode :initarg :visualization-mode :accessor visualization-mode)
   (interface-initial-timer-refresh :initarg :interface-initial-timer-refresh :accessor interface-initial-timer-refresh)
   (interface-timer-refresh-rate :initarg :interface-timer-refresh-rate :accessor interface-timer-refresh-rate)
   (ip-update-timer-initial-delta :initarg :ip-update-timer-initial-delta :accessor ip-update-timer-initial-delta)
   (ip-update-timer-delta :initarg :ip-update-timer-delta :accessor ip-update-timer-delta)
   (pane-positioner-main :initarg :pane-positioner-main :accessor pane-positioner-main)
   (pane-positioner-children :initarg :pane-positioner-children :accessor pane-positioner-children)
   (refresh-remote-properties :initarg :refresh-remote-properties :accessor refresh-remote-properties)
   ;; Integrated applications specific
   (time-variable-step :initarg time-variable-step :accessor time-variable-step)
   (image-steps :initarg :image-steps :accessor image-steps)   
   (rgb-image-steps :initarg :rgb-image-steps :accessor rgb-image-steps)
   (texture-deformation-steps :initarg :texture-deformation-steps :accessor texture-deformation-steps)
   ;; OpenCL
   (opencl-platform :initarg :opencl-platform :accessor opencl-platform)
   (opencl-device :initarg :opencl-device :accessor opencl-device)
   (opencl-compile-options :initarg :opencl-compile-options :accessor opencl-compile-options)
   (max-compute-units :initarg :max-compute-units :initform 0 :accessor max-compute-units)
   (max-work-items-dimensions :initarg :max-work-items-dimensions :initform 0 :accessor max-work-items-dimensions)))


(defmethod initialize-instance :after ((o system-configuration) &rest args)
  "Initialize <o>."
  (load-state o))

(defmethod load-state ((o system-configuration))
  "Load <o> internal state from defaults."
  (progn nil))

(defmethod save-state ((o system-configuration))
  "Save <o> internal state to defaults."
  (progn nil))

(defmethod save-property-state ((o system-configuration) (p property))
  (save-state o))

(defmethod initialize-properties :after ((o system-configuration))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type :default-value "System configuration" 
    :data-type 'string :editor 'text-editor :read-only t :visible nil)
   (:name 'compact-tcp-messages :label "Compact tcp messages" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor :update-callback 'save-property-state
    :category "Network")
   (:name 'main-server :label "Main server" :accessor-type 'accessor-accessor-type :data-type 'string 
    :default-value "geneticexplorer.mine.nu" :editor 'string-editor :update-callback 'save-property-state 
    :category "Network")
   (:name 'animation-enabled :label "Animation" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor :update-callback 'save-property-state
    :category "Graphics")
   (:name 'visualization-mode :label "Visualization mode" :accessor-type 'accessor-accessor-type 
    :data-type 'object :default-value *visualization-mode* :editor 'configurable-copy-list-editor
    :update-callback 'save-property-state :possible-values *possible-visualization-modes*
    :category "Graphics")
   (:name 'pane-positioner-main :label "Pane positioner main" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :possible-values (default-pane-positioners) :editor 'list-editor 
    :default-value (system-get 'any-position) :update-callback 'save-property-state :category "Interface")
   (:name 'pane-positioner-children :label "Pane positioner children" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value (system-get 'near-parent-position-x) :update-callback 'save-property-state
    :possible-values (default-pane-positioners) :editor 'list-editor :category "Interface")
   ;; #TODO: Add these
   (:name 'panes-history-level :label "Panes history level" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 3 :editor 'integer-editor :update-callback 'save-property-state
    :category "Interface")
   (:name 'interface-initial-timer-refresh :label "Panes time refresh" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 100 :editor 'integer-editor :update-callback 'save-property-state
    :category "Interface")
   (:name 'interface-timer-refresh-rate :label "Panes time refresh rate" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 400 :editor 'integer-editor :update-callback 'save-property-state
    :category "Interface")
   (:name 'ip-update-timer-initial-delta :label "Server initial ping time" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 10000 :editor 'integer-editor :update-callback 'save-property-state
    :category "Network")
   (:name 'ip-update-timer-delta :label "Server ping time" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 300000 :editor 'integer-editor :update-callback 'save-property-state
    :category "Network")
   (:name 'refresh-remote-properties :label "Refresh remote properties" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value t :editor 'boolean-editor :update-callback 'save-property-state
    :category "Network")
   ;; Integrated applications specific
   (:name 'time-variable-step :label "Time var step" :accessor-type 'accessor-accessor-type :category "Graphics"
    :data-type 'number :default-value 0.1 :editor 'number-editor :update-callback 'save-property-state)
   (:name 'image-steps :label "Image steps" :accessor-type 'accessor-accessor-type :category "Graphics"
    :data-type 'integer :default-value 50 :editor 'integer-editor :update-callback 'save-property-state)
   (:name 'rgb-image-steps :label "RGB image steps" :accessor-type 'accessor-accessor-type :category "Graphics"
    :data-type 'integer :default-value 50 :editor 'integer-editor :update-callback 'save-property-state)
   (:name 'texture-deformation-steps :label "Texture deformation steps" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 50 :editor 'integer-editor :update-callback 'save-property-state
    :category "Graphics")
   ;; OpenCL
   (:name 'opencl-platform :label "Platform" :accessor-type 'accessor-accessor-type :category "OpenCL"
    :data-type 'list :default-value *default-cl-platform* :possible-values (available-opencl-platforms)
    :update-callback 'reset-opencl-device-for-platform :editor 'list-editor)
   (:name 'opencl-device :label "Device" :accessor-type 'accessor-accessor-type :category "OpenCL"
    :data-type 'list :default-value *default-cl-device* :possible-values (available-opencl-devices *default-cl-platform*)
    :editor 'list-editor :update-callback 'save-property-state)
   (:name 'opencl-compile-options :label "Compiler options" :accessor-type 'accessor-accessor-type :category "OpenCL"
    :data-type 'string :editor 'text-editor :default-value "" :update-callback 'save-property-state)
   ;; OpenCL info properties
   (:name 'max-compute-units :label "Max compute units" :accessor-type 'accessor-accessor-type :category "OpenCL"
    :data-type 'int :default-value (system-configuration-device-max-compute-units) :read-only t)
   (:name 'max-work-items-dimensions :label "Max dimensions" :accessor-type 'accessor-accessor-type :category "OpenCL"
    :data-type 'int :default-value (system-configuration-device-max-work-items-dimensions) :read-only t)))

(defun system-configuration-device-max-compute-units ()
  (if (ocl::opencl-available)
      (device-max-compute-units *default-cl-device*)
    0))

(defun system-configuration-device-max-work-items-dimensions ()
  (if (ocl::opencl-available)
      (device-max-work-items-dimensions *default-cl-device*)
    0))

(defun available-opencl-platforms ()
  (if (ocl::opencl-available)
      (ocl:platforms)))

(defun available-opencl-devices (platform)
  (if (ocl::opencl-available)
      (ocl:devices platform)))

;; #NOTE: must call :update-callback 'save-property-state
(defun reset-opencl-device-for-platform (o property)
  (declare (ignore property))
  (setf *default-cl-device* (opencl-device o)))

(defmethod visualization-mode ((o system-configuration))
  "Answer <o> visualization mode."
  *visualization-mode*)

(defun initialize-system-configuration ()
  (system-add 
   (make-instance 'system-configuration :name 'system-configuration)))

(defmacro configuration-get (name)
  `(,name (system-get 'system-configuration)))
