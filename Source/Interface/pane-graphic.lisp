(defclass pane-graphic (base-pane)
  ((model :initarg :model :initform nil :accessor model)
   (graphic :initarg :graphic :initform nil :accessor graphic)
   (font-lists :initarg :font-lists :initform nil :accessor font-lists)))


;; #CHECK
(defmethod initialize-instance :after ((p pane-graphic)  
                                       &optional &key mdi-interface model interface-mode)
  "Initialize <p>."
  (when (interface p)
    (set-model p model)))

(defmethod interface-class ((p pane-graphic))
  "Answer <p> interface class."
  'interface-pane-graphic)

(defmethod selection ((i interface-pane-graphic))
  "Answer <i> selected object."
  (model (pane i)))

(defmethod set-model ((p pane-graphic) object)
  "Set <p> model to <object>."
  (setf (model p) object)
  (set-title (interface p) (graphic p)))

(defmethod reset-image-buffer ((p pane-graphic))
  (setf (pixmap (interface p)) nil))

(defmethod set-title ((i interface-pane-graphic) object)
  "Set title of <i> to <object>."
  (capi:apply-in-pane-process
   i
   (lambda () 
     (setf (capi:interface-title i) (format nil "~A" (name object))))))

(capi:define-interface interface-pane-graphic (base-interface)
  ((pixmap :initform nil :accessor pixmap)
   (image-heigth :initform 0 :accessor image-heigth)
   (image-width :initform 0 :accessor image-width))
  (:panes 
   (graphic redrawing-with-pixmap :accessor graphic))
  (:layouts
   (main-layout capi:column-layout '(graphic layout-buttons-graphic))
   (layout-buttons-graphic capi:row-layout '()))
   (:default-initargs :best-width 275 :best-height 275 
    :visible-min-width 100 :visible-min-height 100
    :destroy-callback 'destroy-interface-pixmap
    :geometry-change-callback 'update-editor-image
    :title "Graphic editor"))

(defmethod initialize-instance :after ((i interface-pane-graphic) &rest keys)
  (setf (owner (graphic i)) i)
  i)      

(defmethod options-menu-description-graphic ()
  "Answer menu description for pane-graphic instances."
  '(("Copy" pane-graphic-copy-object)
    ("Paste" pane-graphic-paste-object)
    ("Edit" pane-graphic-edit-object)
    ("Edit graphic" pane-graphic-edit-graphic)
    ("Refresh" pane-graphic-refresh)))

(defmethod pane-graphic-copy-object (interface data)
  "Copy <interface> model editor to global clipboard."
  (declare (ignore data))
  (add-to-context *drag-context* :object (model (pane interface))))

(defmethod pane-graphic-paste-object (interface data)
  "Set <interface> model editor from global clipboard."
  (declare (ignore data))
  (setf (model (pane interface))
        (get-from-context *drag-context* :object)))

(defmethod pane-graphic-edit-object (interface data)
  "Open an editor pane with <interface> pane model."
  (declare (ignore data))
  (open-editor-with interface (model (pane interface))))
  
(defmethod pane-graphic-edit-graphic (interface data)
  "Open an editor pane with <interface> graphic model."
  (declare (ignore data))
  (open-editor-with interface (graphic (pane interface))))

(defmethod drawable-object ((pane pane-graphic))
  "Answer the drawable object of <pane>."
  (graphic pane))
