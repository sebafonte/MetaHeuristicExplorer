
(defconstant *title-pane-editor* "Editor")
(defconstant *title-pane-graph* "Tree")
(defconstant *title-pane-image* "Graphics")
(defconstant *title-pane-properties* "Properties")
(defconstant *title-pane-parameters* "Parameters")


(capi::define-interface interface-pane-editor-entity (interface-pane-editor-entity-base)
  ((pixmap :initform nil :accessor pixmap)
   (image-heigth :initform 0 :accessor image-heigth)
   (image-width :initform 0 :accessor image-width)
   (selected-tab :initform *title-pane-editor* :initarg :selected-tab :accessor selected-tab))
  (:panes 
   (tab capi:tab-layout 
        :items (let* ((pane1 (make-instance 'pane-editor-lisp
                                            :pane-menu (lambda (pane object x y) 
                                                         (make-pane-menu-with-submenus 
                                                          pane object x y 
                                                          (options-menu-description pane nil)))))
                      (pane2 (make-instance 'capi:graph-pane   
                                            :roots '("")
                                            :children-function 'children-nodes
                                            :layout-function :top-down
                                            :layout-x-adjust :center
                                            :print-function #'print-node
                                            :visible-min-width 200
                                            :visible-min-height 200
                                            :pane-menu (lambda (pane object x y)
                                                         (make-pane-menu-with-submenus 
                                                          pane object x y 
                                                          (options-menu-description pane object)))))
                      (pane3 (make-instance 'redrawing-with-pixmap))
                      (pane4 (make-instance 'interface-pane-editor-properties))
                      (pane5 (make-instance 'interface-pane-editor-parameters))) 
                 (list (list *title-pane-editor* pane1)
                       (list *title-pane-graph* pane2)
                       (list *title-pane-image* pane3)
                       (list *title-pane-properties* pane4)
                       (list *title-pane-parameters* pane5)))
        :accessor tab
        :print-function 'car
        :visible-child-function 'second)
   (button-apply capi:push-button 
                  :text "Apply" 
                  :callback 'apply-editor-changes 
                  :callback-type :interface-data))
  (:layouts (main capi:column-layout '(tab button-apply)))
  (:default-initargs 
   :best-width 230 
   :best-height 280 
   :title "Editor"
   :destroy-callback 'destroy-interface
   :geometry-change-callback 'update-editor-image))


(defmethod initialize-instance :after ((i interface-pane-editor-entity) &rest keys)
  (let ((selected-tab (assoc :selected-tab (to-pairlist keys))))
    (if selected-tab
        (setf (capi:choice-selection (tab i)) (cadr selected-tab)))))

(defmethod set-model ((i interface-pane-editor-entity) (o t))
  "Set <o> as <i> model."
  (if (evolvablep o) (set-graph-model i o))
  (set-editor-model i o)
  (ensure-selection-image-refresh i o)
  (set-title i o)
  (create-property-editors i))

(defmethod set-model ((i interface-pane-editor-entity) (o base-pane))
  "Set <o> as <i> model."
  (create-property-editors i))

(defmethod editor-index (i name)
  (position name (capi:collection-items (tab i)) 
            :test (lambda (a b) (equal (car b) a))))

(defmethod editor-tab (i name)
  (let ((index (editor-index i name)))
    (if index
        (cadr (aref (capi:collection-items (tab i)) index)))))

(defmethod editor-label (i index)
  (car (aref (capi:collection-items (tab i)) index)))

(defmethod pane-editor ((i interface-pane-editor-entity))
  "Answer <i> editor pane."
  (editor-tab i *title-pane-editor*))

(defmethod pane-graph ((i interface-pane-editor-entity))
  "Answer <i> graphic pane."
  (editor-tab i *title-pane-graph*))

(defmethod pane-image ((i interface-pane-editor-entity))
  "Answer <i> image pane."
  (editor-tab i *title-pane-image*))

(defmethod pane-properties ((i interface-pane-editor-entity))
  "Answer <i> properties pane."
  (editor-tab i *title-pane-properties*))

(defmethod pane-parameters ((i interface-pane-editor-entity))
  "Answer <i> parameters pane."
  (editor-tab i *title-pane-parameters*))

(defmethod create-property-editors ((i interface-pane-editor-entity))
  "Create property editors for <i> selected object."
  (create-tabbed-property-editors i)
  (setf (pane (pane-parameters i)) (pane i))
  (create-property-editors (pane-parameters i) i))

(defun create-tabbed-property-editors (i)
  "Create tabbed property editors for <i> selected object."
  (let* ((pane (pane i))
         (interface-editors (pane-properties i))
         (model (model pane))
         (editors (make-hash-table :test 'equal)))
    ;; Reset pane editors
    (setf (property-editors pane) nil)
    ;; Create property editors
    (if model
        (dolist (property (properties model))
          (if (visible property)
              (let ((new-editor (create-editor-for-property-and-object property model)))
                ;; Add editor to pane and hash table
                (appendf (gethash (category property) editors) (list new-editor))
                (appendf (property-editors pane) (list new-editor))))))
    ;; Add editors to category tabs
    (let* ((keys (sort (sort (keys editors)
                                   (lambda (x y) (or (null x) (null y) (string< x y))))
                             (lambda (x y) (declare (ignore y)) (null x))))
           (items))
      (dolist (key keys)
        (appendf items 
                 (list (list key (make-instance 'capi:column-layout :description (gethash key editors))))))
        (let ((tab-layout (make-instance 'capi:tab-layout
                                         :items items
                                         :visible-child-function 'second
                                         :print-function (lambda (y) 
                                                           (let ((x (car y)))
                                                             (if (null x) "General" (format nil "~A" x)))))))
      ;; Add tab-layout to interface
      (capi:execute-with-interface 
       interface-editors
       #'(setf capi:pane-layout)
       tab-layout
       interface-editors))
    ;; Reset tab (for refresh purpose)
    (capi:execute-with-interface 
     i
     (lambda (&rest args)
       (declare (ignore args))
       (setf (capi:choice-selection (tab i)) (capi:choice-selection (tab i))))))))

(defmethod ensure-selection-image-refresh ((i interface-pane-editor-entity) o)
  "Ensures the image of <o> selected on <i> is refreshed when necessary."
  (if (equal (capi:tab-layout-visible-child (tab i))
             (pane-image i))
      (set-image-model i o (pane-image i))))

