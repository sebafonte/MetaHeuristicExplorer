
(defclass gl-texture-manager (object-with-properties)
  ((textures :initarg :textures :initform (make-hash-table) :accessor textures)
   (registry :initarg :registry :initform (make-hash-table) :accessor registry)
   (texturep :initarg :texturep :initform nil :accessor texturep)))


(defmethod register-texture-from-path ((manager gl-texture-manager) name path)
  "Register a texture with <name> on <manager> located at <path>."
  (let ((texture (load-texture-from-path manager name path)))
    (register-texture manager texture name)))

(defmethod register-texture-from-data ((manager gl-texture-manager) name data width heigth mode)
  "Register a texture with <name> on <manager> located at <path>."
  (let ((texture (load-texture-from-data manager name data width heigth mode)))
    (register-texture manager texture name)))

(defmethod load-texture-from-path ((manager gl-texture-manager) name path)
  (let ((image (gp:read-external-image path)))
    (multiple-value-bind (width height)
        (gp:analyze-external-image image)
      (make-instance 'texture
                     :color-mode opengl:*gl-rgb*
                     :texture-heigth width
                     :texture-width height
                     :contents (subseq (graphics-ports::external-image-data image) 56)))))

(defmethod load-texture-from-data ((manager gl-texture-manager) name data width heigth mode)
  (make-instance 'texture 
                 :texture-heigth width 
                 :texture-width heigth 
                 :contents data 
                 :color-mode mode))

(defmethod register-texture ((manager gl-texture-manager) texture name)
  (setf (gethash name (registry manager))
        (opengl:make-gl-vector :unsigned-8 (* (texture-width texture) (texture-heigth texture) 4)
                               :contents (contents texture))
        (gethash name (textures manager))
        texture))

(defmethod unregister-texture ((manager gl-texture-manager) name)
  "Unregister texture with <name> on <manager>."
  (remhash name (registry manager))
  (remhash name (textures manager)))

(defmethod turn-off-texture ((manager gl-texture-manager))
  "#NOTE: This method should be called within a opengl:rendering-on form over canvas."
  (setf (texturep manager) nil)
  (opengl:gl-disable opengl:*gl-texture-2d*))

(defmethod turn-on-texture ((manager gl-texture-manager) name)
  (let ((texture (gethash name (textures manager)))
        (image (gethash name (registry manager))))
    (setf (texturep manager) t)
    (opengl:gl-pixel-storei opengl:*gl-unpack-alignment* 1)
    (opengl:gl-tex-image2-d 
     opengl:*gl-texture-2d* 0 3 (texture-width texture) (texture-heigth texture) 0 (color-mode texture) 
     opengl:*gl-unsigned-byte* image)
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-wrap-s* opengl:*gl-repeat*)
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-wrap-t* opengl:*gl-clamp*)
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-mag-filter* opengl:*gl-linear*)
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-min-filter* opengl:*gl-linear*)
    (opengl:gl-tex-envi opengl:*gl-texture-env* opengl:*gl-texture-env-mode* opengl:*gl-modulate*)
    (opengl:gl-enable opengl:*gl-texture-2d*)))
 
