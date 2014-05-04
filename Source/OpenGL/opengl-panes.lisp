;;; Global variable for tracking pane-graphic instances
(setf *interface-graphic-editors* nil)


;;; This extension can use the following 'interface-mode modes:
;;;
;;;  'interface-pane-editor-entity-opengl
;;;  'interface-pane-editor-entity-image-opengl
;;;

(defclass pane-editor-entity-opengl (pane-editor-entity)
  ())

(defmethod apply-changes :after ((o pane-editor-entity-opengl))
  "Updates <o> for any possible change."
  (reset-image o))

(defmethod interface-class ((p pane-editor-entity-opengl))
  "Answer <p> interface class."
  (interface-mode p))

(defmacro make-graphic-pane (&rest args)
  "Default editor-pane constructor macro with <args>."
  `(make-instance 
    (graphic-pane-class *visualization-mode*)
    ,@args))

(defmacro make-editor-pane (&rest args)
  "Default editor-pane constructor macro with <args>."
  `(make-instance 
    (editor-pane-class *visualization-mode*)
    :interface-mode (editor-pane-interface-class *visualization-mode*)
    ,@args))

(defmacro make-image-editor-pane (&rest args)
  "Default image-editor-pane constructor macro with <args>."
  `(make-instance 
    (editor-pane-class *visualization-mode*)
    :interface-mode (editor-pane-interface-image-class *visualization-mode*)
    ,@args))

(defmacro make-entity-explorer-pane (&rest args)
  "Default mutation-pane constructor macro with <args>."
  `(make-instance 
    'pane-editor-entity-explorer
    :interface-mode (mutation-editor-pane-interface-image-class *visualization-mode*)
    ,@args))

(defun initialize-editors-refresh-process ()
  (let ((timer (mp:make-timer 
                (lambda () 
                  (dolist (i *interface-editors*)
                    (redisplay-canvas (graphic-part i)))))))
    (mp:schedule-timer-milliseconds timer 0 0.05)))


(capi:define-interface interface-pane-graphic-opengl (base-interface)
  ((camera :initarg :camera :initform (make-camera) :accessor camera)
   (image-heigth :initform 0 :accessor image-heigth)
   (image-width :initform 0 :accessor image-width))
  (:panes 
   (graphic capi:opengl-pane
            :configuration (list :rgba t :depth nil :double-buffered t)
            :display-callback 'redisplay-canvas
            :resize-callback 'resize-canvas
            :pane-menu (lambda (pane object x y)
                         (make-pane-menu-with-submenus 
                          pane object x y 
                          (options-menu-description-graphic)))
            :input-model `(((:button-1 :press)
                            ,#'(lambda (pane x y)
                                 (drag-from-pane pane x y)
                                 (pane-toggle-mouse-cursor-on-drag pane x y)))
                           ((:button-1 :release)
                            ,#'(lambda (pane x y)
                                 (drop-from-pane pane x y)
                                 (pane-toggle-mouse-cursor-normal pane x y))))
            :font (gp:make-font-description :family "Arial")
            :accessor graphic)
   (simple-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 29 :selected-image 29
                                :help-key "Graphic class"
                                :selection-callback 'graphic-pane-graphic-class-callback
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 9 :selected-image 9
                                :help-key "Graphic properties"
                                :selection-callback 'graphic-pane-graphic-properties-callback
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 25 :selected-image 25
                                :help-key "Turn animation"
                                :selection-callback 'graphic-pane-turn-animation-callback
                                :callback-type :interface-data))
           :selection nil))
    :callback-type :interface-data
    :default-image-set (capi:make-general-image-set :id 'global-button-icons-images :image-count 3)))
  (:layouts
   (main-layout capi:column-layout '(layout-graphic-toolbar graphic))
   (layout-graphic-toolbar capi:row-layout '(simple-toolbar)))
   (:default-initargs :best-width 275 :best-height 275 
    :visible-min-width 100 :visible-min-height 100
    :title "Graphic editor"
    :destroy-callback 'destroy-interface))


(defmethod graphic-part ((interface interface-pane-graphic-opengl))
  (graphic interface))

(defmethod destroy-interface ((i interface-pane-graphic-opengl))
  "Perform actions when <interface> is destroyed."
  (setf *interface-graphic-editors* 
        (reject *interface-graphic-editors* (lambda (object) (equal object i)))))

(defmethod initialize-instance :after ((i interface-pane-graphic-opengl) &rest keys)
  "Initialize <i>."
  (appendf *interface-graphic-editors* (list i)))

(defclass pane-graphic-opengl (pane-graphic)
  ())


(defmethod interface-class ((p pane-graphic-opengl))
  "Answer <p> interface class."
  'interface-pane-graphic-opengl)

(defmethod set-title ((i interface-pane-graphic-opengl) object)
  "Set title of <i> to <object>."
  (capi:apply-in-pane-process
   i
   (lambda () 
     (setf (capi:interface-title i) (format nil "~A" (name object))))))

(defun graphic-pane-graphic-class-callback (interface data)
  nil)

(defun graphic-pane-graphic-properties-callback (interface data)
  (open-editor-with interface (graphic (pane interface))))

(defun graphic-pane-turn-animation-callback (interface data)
  nil)
