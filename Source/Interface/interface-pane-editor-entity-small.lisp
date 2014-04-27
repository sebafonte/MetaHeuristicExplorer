
(capi::define-interface interface-pane-editor-entity-small (interface-pane-editor-entity)
  ((pixmap :initform nil :accessor pixmap)
   (image-heigth :initform 0 :accessor image-heigth)
   (image-width :initform 0 :accessor image-width))
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
                                            :visible-min-width 80
                                            :visible-min-height 80
                                            :pane-menu (lambda (pane object x y)
                                                         (make-pane-menu-with-submenus 
                                                          pane object x y 
                                                          (options-menu-description pane object)))))
                      (pane3 (make-instance 'redrawing-with-pixmap))
                      (pane4 (make-instance 'interface-pane-editor-properties))
                      (pane5 (make-instance 'interface-pane-editor-parameters))) 
                 (list (list "Editor" pane1)
                       (list "Tree" pane2)
                       (list "Graphic" pane3)
                       (list "Properties" pane4)
                       (list "Parameters" pane5)))
        :accessor tab
        :print-function 'car
        :visible-child-function 'second)
   (button-apply capi:push-button 
                  :text "Apply" 
                  :callback 'apply-editor-changes 
                  :callback-type :interface-data))
  (:layouts (main capi:column-layout '(tab button-apply)))
  (:default-initargs 
   :selected-tab nil
   :best-width 80 
   :best-height 80 
   :title "Editor"
   :destroy-callback 'destroy-interface
   :geometry-change-callback 'update-editor-image))