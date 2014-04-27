
(defclass list-check-editor (property-editor interface-check-list-editor)
  ((selections :initarg :selections :initform nil :accessor selections)
   (subject :initarg :subject :initform nil :accessor subject)
   (property :initarg :property :initform nil :accessor property)))


(defmethod initialize-instance :after ((object list-check-editor) &key selections)
  "Initialize <object>."
  (declare (ignore selections))
  (let* ((pane-description (list-check-editor-checks-description object))
         (column-layout (make-instance 'capi:column-layout :description pane-description)))
    (capi:execute-with-interface
     object
     (lambda (&rest args) 
       (declare (ignore args))
       (setf (capi:titled-object-title object) (label (property object))
             (capi:pane-layout object) column-layout)))))

(defmethod list-check-editor-checks-description ((object list-check-editor))
  "Anser a list of editors for <object>."
  (mapcar (lambda (o)
            (make-instance 'capi:check-button 
                           :text (format nil "~A" o) 
                           :data o
                           :selected (find o (get-value-for-property (subject object) (property object)))))
          (possible-values (property object))))

(defmethod interface-selections ((object list-check-editor))
  "Answer the selected items of <object>."
  (let ((result))
    (dolist (item (capi:layout-description (capi:pane-layout object)))
      (if (capi:button-selected item)
          (appendf result (list (capi:item-data item)))))
    result))