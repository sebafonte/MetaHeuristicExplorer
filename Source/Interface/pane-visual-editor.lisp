(defclass pane-visual-editor (base-pane)
  ((models :initarg :models :initform nil :accessor models)
   (pane-operation :initarg :pane-operation :initform nil :accessor pane-operation)))


(capi:define-interface interface-pane-visual-editor (base-interface)
  ()
  (:panes
   (combo-individual-operation capi:option-pane 
                               :title "Individual operation: "
                               :items (unary-genetic-operators))
   (combo-dragdrop-operation capi:option-pane 
                             :title "Drag & drop operation: "
                             :items (binary-genetic-operators))
   (object-a capi:graph-pane   
             :roots '("")
             :children-function 'children-nodes
             ;:node-pinboard-class 'capi:push-button
             ;:node-pane-function #'tipo-nodo
             ;:layout-function :left-right
             :layout-function :top-down
             :layout-x-adjust :center
             :print-function #'print-node)
   (object-b capi:graph-pane   
             :roots '("")
             :children-function 'children-nodes
             ;:node-pinboard-class 'capi:push-button
             ;:node-pane-function #'tipo-nodo
             ;:layout-function :left-right
             :layout-function :top-down
             :layout-x-adjust :center
             :print-function #'print-node))
  (:layouts
   (main-layout capi:column-layout '(combo-individual-operation
                                     combo-dragdrop-operation
                                     muestras))
   (muestras capi:row-layout '(object-a object-b)))
  (:default-initargs 
   :best-width 700 :best-height 500 
   :title "Visual object editor"))

(defmethod create-interface ((p pane-visual-editor))
  (make-instance 'interface-pane-visual-editor))

(defmethod add-object ((p pane-visual-editor) object)
  "Add <object> to <p> and refresh <p>."
  (append models (list object)))

(defmethod delete-object ((p pane-visual-editor) object)
  "Delete <object> from <p> and refresh <p>."
  (remove object (models p)))

(defmethod clean ((p pane-visual-editor))
  "Clean <p>."
  (setf (models p) nil))

(defmethod operate ((p pane-visual-editor) objects)
  "Operate on <p> over its <objects>."
  nil)
