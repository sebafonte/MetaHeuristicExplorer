
(capi::define-interface interface-pane-editor-entity-image (interface-pane-editor-entity-base)
  ((pixmap :initform nil :accessor pixmap)
   (image-heigth :initform 0 :accessor image-heigth)
   (image-width :initform 0 :accessor image-width))
  (:panes 
   (pane-image redrawing-with-pixmap :accessor pane-image))
  (:layouts (main capi:column-layout '(pane-image)))
  (:default-initargs 
   :best-width 200 :best-height 460 
   :visible-min-width 70
   :visible-min-height 70
   :display-state :internal-borderless
   :destroy-callback 'destroy-interface
   :geometry-change-callback 'update-editor-image))


(defmethod set-model ((i interface-pane-editor-entity-image) (p t))
  "Set <p> as <i> model."
  (set-image-model i p (pane-image i)))

(defmethod create-property-editors ((i interface-pane-editor-entity-image))
  "Create property editors for <i>."
  nil)

(defmethod set-editor-tab-to ((i interface-pane-editor-entity-image) number)
  "Set tab numbered <number> on <i>."
  nil)
