

(defclass visualization-mode (object-with-properties)
  ((render-quality :initarg render-quality :initform 10 :accessor render-quality)))

  
(defmethod initialize-properties :after ((o visualization-mode))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'render-quality :label "Render quality (0-10)" :accessor-type 'accessor-accessor-type 
    :default-value "System configuration" :data-type 'integer :editor 'integer-editor)))

(defmethod default-editor-pane-interface-class ((o visualization-mode))
  (editor-pane-interface-class o))
           

(defclass native-capi (visualization-mode)
  ())

(defmethod initialize-properties :after ((o native-capi))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'property-accessor-type :default-value "CAPI Native" 
    :data-type 'string :editor 'text-editor :read-only t)))

(defmethod editor-pane-class ((o native-capi))
  'pane-editor-entity)

(defmethod editor-pane-interface-class ((o native-capi))
  'interface-pane-editor-entity)

(defmethod editor-pane-interface-image-class ((o native-capi))
  'interface-pane-editor-entity-image)

(defmethod mutation-editor-pane-interface-image-class ((o native-capi))
  'interface-pane-editor-entity-explorer)

(defmethod graphic-pane-class ((o native-capi))
  'pane-graphic)

(defmethod refresh-editors ((o native-capi))
  (dolist (editor *interface-editors*)
    (let* ((part (graphic-part editor))
           (pixmap (pixmap-object (capi:element-interface part))))
      (capi:with-geometry pixmap 
        (draw-interface-pixmap part pixmap capi:%x% capi:%y% nil nil)))))


(defclass opengl-with-capi (visualization-mode)
  ())

(defmethod initialize-properties :after ((o opengl-with-capi))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'property-accessor-type :default-value "OpenGL with CAPI" 
    :data-type 'string :editor 'text-editor :read-only t)))

(defmethod editor-pane-class ((o opengl-with-capi))
  'pane-editor-entity-opengl)

(defmethod editor-pane-interface-class ((o opengl-with-capi))
  'interface-pane-editor-entity-opengl)

(defmethod editor-pane-interface-image-class ((o opengl-with-capi))
  'interface-pane-editor-entity-image-opengl)

(defmethod mutation-editor-pane-interface-image-class ((o opengl-with-capi))
  'interface-pane-editor-entity-explorer-opengl)

(defmethod graphic-pane-class ((o opengl-with-capi))
  'pane-graphic-opengl)

(defmethod refresh-editors ((o opengl-with-capi))
  (dolist (editor *interface-editors*)
    (redisplay-canvas (graphic-part editor)))
  (dolist (editor *interface-graphic-editors*)
    (redisplay-canvas (graphic-part editor))))
