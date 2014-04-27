
(capi:define-interface interface-object-choice-select (base-interface)
  ()
  (:panes
   (list-panel capi:list-panel :items '("Object-1" "Object-2" "Object-3") :selection 0 :accessor list-panel)
   (button-edit capi:push-button :text "Edit")
   (button-select capi:push-button :text "Select" :callback 'pane-choice-select-selection)
   (button-cancel capi:push-button :text "Cancel"))
  (:layouts
   (column-layout-1 capi:column-layout '(list-panel row-layout-1))
   (row-layout-1 capi:row-layout '(button-edit button-select button-cancel)))
  (:default-initargs
   :best-height 400
   :best-width 200
   :layout 'column-layout-1
   :title "Object selection"))


(defun pane-choice-select-selection (data interface)
  (declare (ignore data) (ignore interface))
  nil)

(defmethod selection ((i interface-object-choice-select))
  (capi:selection (list-panel i)))

(defun prompt-for-default-object-choices (message list)
  ;; #NOTE: This will be used later
  (let* ((selection-interface (make-instance 'interface-object-choice-select))
         (interface (list-panel selection-interface)))
    ;; Set items
    (capi:apply-in-pane-process
     interface 
     #'(setf capi:collection-items) 
     list
     interface)
    ;; Open dialog
    (capi:prompt-with-list list message :interaction :multiple-selection)))