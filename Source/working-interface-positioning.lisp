

(defmethod create-editor-for-editor-class ((editor-class (eql 'button-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it´s data type."
  (make-instance editor-class 
                 :title (label p) 
                 :text "" 
                 :callback (lambda (interface data) 
                             (declare (ignore data))
                             (if (get-value-for-property o p) 
                                 (open-in-new-positioned-editor (get-value-for-property o p) interface)))
                 :callback-type :interface-data
                 :subject o))

(defmethod open-in-new-positioned-editor ((o t) interface)
  "Open a new editor with <o> as it's model on <interface>."
  (let ((editor (make-editor-pane :model o 
                                  :mdi-interface interface 
                                  ;:open nil 
                                  ;:prepare nil
                                  :positioner (make-instance 'near-parent-position :parent-interface interface))))
    (set-model editor o)
    (initialize-interface editor)
    ;(open-pane editor :mdi-interface (interface *main-pane*))
    (set-position (positioner editor) (interface editor))))

#|
(defmethod open-in-new-positioned-editor ((o t) interface)
  "Open a new editor with <o> as it's model on <interface>."
  (let ((editor (make-editor-pane :model o 
                                  :mdi-interface interface 
                                  :open nil 
                                  ;:prepare nil
                                  :positioner (make-instance 'near-parent-position :parent-interface interface))))
    (set-model editor o)
    (initialize-interface editor)
    (set-position (positioner editor) (interface editor))
    (open editor :mdi-interface (interface *main-pane*))))
|#

(defmethod open ((p base-pane) &key mdi-interface)
  "Display <p> on default GUI."
  (display-interface p mdi-interface)
  (post-initialize-interface p)
  (customize-interface p mdi-interface))

(defmethod initialize-interface ((p base-pane))
  (set-initial-position (positioner p) (interface p))
  (setf (interface p) (create-interface p)
        (pane (interface p)) p))

(defmethod set-initial-position ((o near-parent-position) interface)
  "Set <interface> position depending on <pane-interface> and <o> strategy."
  (multiple-value-bind (x y width height)
      (capi:top-level-interface-geometry (capi:top-level-interface (parent-interface o)))
    (appendf (interface-arguments (pane interface)) 
             (list :x (+ x width (delta-x o)) 
                   :y (+ y (delta-y o)))))) 

(defun option-editable-callback (interface data)
  "Open a new editor over the model of interface."
  (declare (ignore data))
  (open-in-new-positioned-editor (subject interface) interface))
 