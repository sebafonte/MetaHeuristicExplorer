
(capi:define-interface option-editable-pane ()
  ()
  (:panes
   (option-pane
    capi:option-pane
    :accessor option-pane
    :title ""
    :selection-callback 'update-value-callback
    :callback-type :interface-data
    :font (gp:make-font-description :size 7))
   (button-pane
    button-editor
    :text ""
    :accessor button-pane
    :callback 'option-editable-callback 
    :callback-type :interface-data))
  (:layouts
   (row-layout capi:row-layout '(option-pane button-pane)))
  (:default-initargs
   :best-height nil
   :best-width nil
   :layout 'row-layout))


(defun option-editable-callback (interface data)
  "Open a new editor on <interface> model."
  (declare (ignore data))
  (open-in-new-editor (subject interface) interface))

(defun update-value-callback (interface data)
  "Update <interface> model when selection updated."
  (setf (subject interface) (copy-cyclic data))
  (apply-property-values (pane (capi:element-interface (capi:element-interface interface)))))


(defclass configurable-copy-list-editor (option-editable-pane property-editor)
  ((subject :initarg :subject :initform nil :accessor subject)))


(defmethod initialize-instance :after ((object configurable-copy-list-editor) &key items selected-item enabled label)
  (declare (ignore enabled))
  (let ((option-pane (option-pane object)))
    (capi:execute-with-interface object #'(setf capi:titled-object-title) label option-pane)
    (capi:execute-with-interface object #'(setf capi:collection-items) items option-pane)
    (capi:execute-with-interface object #'(setf capi:choice-selected-item) selected-item option-pane)))

(defmethod value ((editor configurable-copy-list-editor))
  (subject editor))

;; Creation
(defmethod create-editor-for-editor-class ((editor-class (eql 'configurable-copy-list-editor)) (p property) (o object-with-properties))
  (let ((value (get-value-for-property o p)))
    (make-instance editor-class 
                   :label (label p) 
                   :items (append (list value) (possible-values p))
                   :selected-item value
                   :enabled (not (read-only p))
                   :subject value)))