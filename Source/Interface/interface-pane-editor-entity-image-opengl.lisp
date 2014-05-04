
(capi::define-interface interface-pane-editor-entity-base-opengl 
    (interface-pane-editor-entity-base)
  ())

(capi::define-interface interface-pane-editor-entity-image-opengl 
    (interface-pane-editor-entity-base-opengl)
  ((owner :initarg :owner :accessor owner)
   (double-buffered-p :initarg :double-buffered-p :initform t :accessor double-buffered-p)
   (camera :initarg :camera :initform (make-camera) :accessor camera)
   (transform :initarg :transform :initform nil :accessor transform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform))
  (:panes 
   (pane-image capi:opengl-pane
               :configuration (list :rgba t :depth nil :double-buffered t)
               :display-callback 'redisplay-canvas
               :resize-callback 'resize-canvas
               :pane-menu (lambda (pane object x y)
                            (make-pane-menu-with-submenus 
                             pane object x y 
                             (options-menu-description pane object)))
               :input-model `(((:button-1 :press)
                               ,#'(lambda (pane x y)
                                    (drag-from-pane pane x y)
                                    (pane-toggle-mouse-cursor-on-drag pane x y)))
                              ((:button-1 :release)
                               ,#'(lambda (pane x y)
                                    (drop-from-pane pane x y)
                                    (pane-toggle-mouse-cursor-normal pane x y))))
               :font (gp:make-font-description :family "Arial")))
  (:layouts (main capi:column-layout '(pane-image)))
  (:default-initargs 
   :best-width 200 :best-height 200 
   :visible-min-width 70
   :visible-min-height 70
   :display-state :internal-borderless
   :destroy-callback 'destroy-interface))

(capi::define-interface interface-pane-editor-entity-opengl 
    (interface-pane-editor-entity-base-opengl)
  ((owner :initarg :owner :accessor owner)
   (double-buffered-p :initarg :double-buffered-p :initform t :accessor double-buffered-p)
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
                      (pane3 (make-instance 'capi:opengl-pane
                                            :configuration (list :rgba t :depth nil :double-buffered t)
                                            :min-width 200
                                            :min-height 200
                                            :display-callback 'redisplay-canvas
                                            :resize-callback 'resize-canvas
                                            :pane-menu (lambda (pane object x y)
                                                         (make-pane-menu-with-submenus 
                                                          pane object x y 
                                                          (options-menu-description pane object)))
                                            :input-model `(((:button-1 :press)
                                                            ,#'(lambda (pane x y)
                                                                 (drag-from-pane pane x y)
                                                                 (pane-toggle-mouse-cursor-on-drag pane x y)))
                                                           ((:button-1 :release)
                                                            ,#'(lambda (pane x y)
                                                                 (drop-from-pane pane x y)
                                                                 (pane-toggle-mouse-cursor-normal pane x y))))
                                            :font (gp:make-font-description :family "Arial")))
                      (pane4 (make-instance 'interface-pane-editor-properties))
                      (pane5 (make-instance 'interface-pane-editor-parameters))) 
                 (list (list "Editor" pane1)
                       (list "Tree" pane2)
                       (list "Graphics" pane3)
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
   :best-width 230 
   :best-height 280 
   :title "Editor"
   :destroy-callback 'destroy-interface))


(defmethod set-editor-tab-to ((i interface-pane-editor-entity-image-opengl) number)
  "Set tab numbered <number> on <i>."
  nil)

(defmethod initialize-viewer ((i interface-pane-editor-entity-base-opengl))
  nil)

(defmethod destroy-interface ((i interface-pane-editor-entity-base-opengl))
  "Perform actions when <i> is destroyed."
  nil)

(defmethod pane-editor ((i interface-pane-editor-entity-opengl))
  "Answer <i> editor pane."
  (editor-tab i *title-pane-editor*))

(defmethod pane-graph ((i interface-pane-editor-entity-opengl))
  "Answer <i> graph pane."
  (editor-tab i *title-pane-graph*))

(defmethod pane-image ((i interface-pane-editor-entity-opengl))
  "Answer <i> image pane."
  (editor-tab i *title-pane-image*))

(defmethod pane-properties ((i interface-pane-editor-entity-opengl))
  "Answer <i> properties pane."
  (editor-tab i *title-pane-properties*))

(defmethod pane-parameters ((i interface-pane-editor-entity-opengl))
  "Answer <i> parameters pane."
  (editor-tab i *title-pane-parameters*))

(defmethod graphic-part ((i interface-pane-editor-entity-opengl))
  (pane-image i))

(defmethod graphic-part ((i interface-pane-editor-entity-image-opengl))
  (first (capi::interface-panes i)))
