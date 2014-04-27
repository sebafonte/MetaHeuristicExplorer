
#|
(opengl:gl-call-list (display-list object))
(opengl:gl-delete-lists (display-list object) 1)

(let ((n (opengl:gl-gen-lists 1)))
  (if (plusp n)
      (progn
        (opengl:gl-new-list n opengl:*gl-compile-and-execute*)
        (opengl:gl-end-list))
    (error "No more display list indexes!")))

(opengl:gl-shade-model opengl:*gl-smooth*)

(when texturep
  (opengl:gl-enable opengl:*gl-texture-gen-s*)
  (opengl:gl-enable opengl:*gl-texture-gen-t*)
  (texgen opengl:*gl-s* 1.0 0.0 0.0 1.0)
  (texgen opengl:*gl-t* 0.0 1.0 0.0 1.0)
  (opengl:gl-color4-fv (get-texture-color)))

(opengl:gl-color4-fv (aref colors ii))

(opengl:gl-begin opengl:*gl-polygon*)
(opengl:gl-normal3-dv v)
(opengl:gl-vertex4-dv v)
(opengl:gl-end)

(when texturep
  (opengl:gl-disable opengl:*gl-texture-gen-s*)
  (opengl:gl-disable opengl:*gl-texture-gen-t*))

(defun initialize-viewer (icosahedron-viewer)
  ;; Initialize the icotransform to unity.
  (opengl:rendering-on ((canvas icosahedron-viewer))
    (setf (icotransform icosahedron-viewer) (make-gl-double-vector 16))
    (setf (light-transform icosahedron-viewer) (make-gl-double-vector 16))
    (initialize-transform (icotransform icosahedron-viewer))
    (initialize-transform (light-transform icosahedron-viewer))
    (reset-lights-and-materials)))

(defmethod initialize-instance :after ((self icosahedron-viewer) &key &allow-other-keys)
  (setf (viewer (object self)) self))

(defun turn-off-texture (viewer)
  (opengl:rendering-on ((canvas viewer))
    (setf (texturep (icosahedron viewer)) nil
          (texturep viewer) nil)
    (opengl:gl-disable opengl:*gl-texture-2d*)))
|#

(defun initialize-transform (transform)
  (opengl:gl-matrix-mode opengl:*gl-modelview*)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defmethod displayed-object ((p pane-editor-entity))
  (model p))

(defmethod displayed-object ((p pane-graphic))
  (graphic p))

(defun redisplay-canvas (canvas &rest ignore)
  (let ((viewer (capi:element-interface canvas)))
    (when viewer
      (let ((selection (displayed-object (pane viewer))))
        (initialize-viewer viewer)
        (when selection
          (draw-opengl-on selection canvas viewer))))))

(defmethod initialize-viewer ((i base-interface))
  nil)

(defun resize-canvas (canvas x y width height)
  x y
  (when #+Win32 (win32:is-window-visible (win32:pane-hwnd (capi-internals:representation canvas)))
	#-Win32 T
        (opengl:rendering-on (canvas)
          (opengl:gl-viewport 0 0 width height))
          ;(with-slots ((viewer capi:interface)) canvas
          ; (setf (aspect (projection (camera viewer)))
          ;       (coerce (/ width height) 'double-float))
          ;)
        (redisplay-canvas canvas)))
 
(defmethod set-model ((p pane-editor-entity-opengl) o)
  "Set <p> model to <o> and refresh <p> interface."
  (setf (slot-value p 'model) o)
  (set-model (interface p) o))

(defmethod create-property-editors ((p pane-editor-entity-opengl))
  "Create <p> model property editors on <p> interface."
  (if (model p) 
      (create-property-editors (interface p))))
