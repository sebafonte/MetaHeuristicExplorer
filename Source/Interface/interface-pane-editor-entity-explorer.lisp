
(capi::define-interface interface-pane-editor-entity-explorer (interface-pane-editor-entity-base)
  ((pixmap :initform nil :accessor pixmap)
   (image-heigth :initform 0 :accessor image-heigth)
   (image-width :initform 0 :accessor image-width))
  (:panes
   (column-layout-1
    capi:column-layout
    :description (list 
                  ;; Pane lisp text editor
                  (make-instance 'pane-editor-lisp
                                 :pane-menu (lambda (pane object x y) 
                                              (make-pane-menu-with-submenus 
                                               pane object x y 
                                               (options-menu-description pane nil)))))
    :accessor column-layout-1)
   (column-layout-2
    capi:column-layout
    :description (list
                  ;; Pane tree editor
                  (make-instance 'capi:graph-pane   
                                 :roots '("")
                                 :children-function 'children-nodes
                                 :layout-function :top-down
                                 :layout-x-adjust :center
                                 :print-function #'print-node
                                 :visible-min-width 200
                                 :visible-min-height 200
                                 :pane-menu (lambda (pane object x y)
                                              (make-pane-menu 
                                               pane object x y 
                                               (options-menu-description pane object)))
                                 :input-model '(((:button-1 :second-press) 
                                                 entity-explorer-button-action-callback)
                                                ((:button-3 :press) 
                                                 entity-explorer-button-press-callback)
                                                ((:button-1 :shift :second-press) 
                                                 entity-explorer-button-shift-action-callback)))
                  ;; Pane property editor
                  (make-instance 'interface-pane-editor-properties))
    :accessor column-layout-2)
   (column-layout-3
    capi:column-layout
    :description (list
                  ;; Pane with model
                  (make-image-editor-pane :open nil)
                  ;; Pane with children
                  (make-instance 'capi:grid-layout 
                                 :description 
                                 (loop for i below 20 collect 
                                       (interface (make-image-editor-pane :open nil)))
                                 :columns 4))
    :accessor column-layout-3))
  (:layouts
   (main-layout capi:row-layout '(column-layout-2 column-layout-3)))
  (:default-initargs
   :visible-min-height 300 :visible-min-width 600
   :layout 'main-layout
   :destroy-callback 'destroy-interface-pixmap
   :geometry-change-callback 'update-editor-image
   :title "Editor"))


(defmethod create-property-editors ((i interface-pane-editor-entity-explorer))
  "Create property editors for visible properties of <i> selection."
  (let* ((pane (pane i))
         (model pane)
         (interface-editors (pane-properties i)))
    ;; Create editors list
    (setf (property-editors pane) nil)
    (if model
        (dolist (property (properties-definition model))
          (if (visible property)
              (appendf (property-editors pane) 
                       (list (create-editor-for-property-and-object property model))))))
    ;; Add editor to interface
    (capi:execute-with-interface 
     interface-editors
     #'(setf capi:pane-layout)
     (make-instance 'capi:column-layout :description (property-editors pane))
     interface-editors)))

(defmethod pane-editor ((i interface-pane-editor-entity-explorer))
  "Answer <i> editor pane."
  (first (capi:layout-description (column-layout-1 i))))

(defmethod pane-properties ((i interface-pane-editor-entity-explorer))
  "Answer <i> properties pane."
  (second (capi:layout-description (column-layout-2 i))))

(defmethod pane-graph ((i interface-pane-editor-entity-explorer))
  "Answer <i> graph pane."
  (first (capi:layout-description (column-layout-2 i))))

(defmethod pane-image ((i interface-pane-editor-entity-explorer))
  "Answer <i> image pane."
  (first (capi:layout-description (column-layout-3 i))))

(defmethod pane-children ((i interface-pane-editor-entity-explorer))
  "Answer <i> children pane."
  (second (capi:layout-description (column-layout-3 i))))

(defmethod set-model ((i interface-pane-editor-entity-explorer) (o t))
  "Set <o> as <i> model."
  (set-editor-model i o)
  (if (evolvablep o) (set-graph-model i o))
  (create-property-editors i)
  (set-title i o)
  (set-model (pane (pane-image i)) o))

(defmethod set-model ((i interface-pane-editor-entity-explorer) (o base-pane))
  "Set <o> as <i> model."
  (declare (ignore o))
  (create-property-editors i))

;; #TODO: Generalize this function and the next below
(defmethod remap-population-interface ((interface interface-pane-editor-entity-explorer))
  "Update pane objects from <interface>."
  (let ((children (children (pane interface)))
        (pane-children (pane-children interface)))
    (dotimes (i (size children))
      (setf (aref (individuals-array children) i) 
            (model (pane (nth i (capi:layout-description pane-children))))))))

(defmethod remap-interface-population ((interface interface-pane-editor-entity-explorer))
  "Update pane objects from <interface>."
  (let ((children (children (pane interface)))
        (pane-children (pane-children interface)))
    (dotimes (i (size children))
      (set-model (pane (nth i (capi:layout-description pane-children))) 
                 (aref (individuals-array children) i)))))
