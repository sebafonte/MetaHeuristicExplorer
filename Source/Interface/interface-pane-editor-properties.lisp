
(capi:define-interface interface-pane-editor-properties (base-interface)
  ())

(capi:define-interface interface-pane-editor-parameters (base-interface)
  ((editors :initarg :editors :initform nil :accessor editors))
  (:panes
   (slider capi:slider)
   (boolean-editor boolean-editor :text "Boolean editor"))
  (:layouts
   (column-layout-1 capi:column-layout '(row-layout-1 column-layout-2))
   (row-layout-1 capi:row-layout '(slider))
   (column-layout-2 capi:column-layout '(boolean-editor)))
  (:default-initargs :best-height nil :best-width nil :layout 'column-layout-1))


(defmethod set-model ((i interface-pane-editor-parameters) (object t))
  "Set <object> as <i> model."
  (declare (ignore object))
  (create-property-editors i))

(defmethod create-property-editors ((i interface-pane-editor-parameters))
  "Create property editors for <i>."
  (create-parameter-editors i))

(defmethod create-parameter-editors ((i interface-pane-editor-parameters))
  "Create editors for <i> parameter properties."
  (let* ((pane (pane i))
         (model (model pane))
         (editors))
    ;; Look for parametrizable properties
    (when model
      (dolist (property (properties model))
        (when (object-parameter property)
          (appendf editors (list (create-parameter-editor property model))))))
    ;; Set parameter editors on interface
    (setf (editors i) editors)
    (capi:execute-with-interface 
     i
     #'(setf capi:pane-layout)
     (make-instance 'capi:column-layout :description editors)
     i)))

(defun create-parameter-editor (property object)
  (create-editor-for-editor-class
   'editable-parameter-editor
   property
   object))

