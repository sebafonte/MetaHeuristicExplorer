
(capi::define-interface interface-pane-editor-entity-explorer-opengl
    (interface-pane-editor-entity-explorer)
  ()
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
                  (interface (make-image-editor-pane :open nil))
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
   :destroy-callback 'destroy-interface
   :geometry-change-callback 'update-editor-image
   :title "Editor"))


(defmethod set-image-model ((i interface-pane-editor-entity-explorer-opengl) object image-editor)
  "Refresh <i> image."
  nil)

;; #TODO: Check if necessary
(defmethod graphic-part ((i interface-pane-editor-entity-explorer-opengl))
  (pane-image i))

(defmethod connect-interface ((p pane-editor-entity-explorer))
  "Connect pane-interface events for <p>."
  (connect-parent-model-event p))

(defmethod connect-parent-model-event ((p pane-editor-entity-explorer))
  (let* ((interface (interface p))
         (pane-image (pane-image interface)))
    (when-send-to pane-image 
                  :interface-model-changed 
                  (lambda (p pane-image)
                    (parent-gene-editor-model-changed-callback p pane-image))
                  p)))

(defun parent-gene-editor-model-changed-callback (p pane-image)
  (set-model p (model (pane pane-image))))

(defmethod set-model ((i interface-pane-editor-entity-opengl) (o t))
  "Set <o> as <i> model."
  (when (evolvablep o) 
    (set-graph-model i o))
  (set-editor-model i o)
  (set-title i o)
  ;;(let ((tab (capi:tab-layout-visible-child-function i)))
    (create-property-editors i)
    ;;(setf (capi:tab-layout-visible-child-function i) tab))
  (when (pane-image i)
    (redisplay-canvas (graphic-part i))))

(defmethod set-model ((i interface-pane-editor-entity-image-opengl) (o t))
  "Set <o> as <i> model."
  (redisplay-canvas (graphic-part i)))

(defmethod create-property-editors ((i interface-pane-editor-entity-opengl))
  (when (pane-properties i)
    (create-tabbed-property-editors i)))

(defmethod create-parameter-editors ((i interface-pane-editor-entity-opengl))
  (when (pane-parameters i)
    (setf (pane (pane-parameters i)) (pane i))
    (create-property-editors (pane-parameters i))))

(defmethod set-editor-tab-to ((i INTERFACE-PANE-EDITOR-ENTITY-EXPLORER-OPENGL) number)
  "Set tab numbered <number> on <i>."
  nil)

(defmethod tab ((i INTERFACE-PANE-EDITOR-ENTITY-EXPLORER-OPENGL))
  "Answer <i> tab."
  nil)
