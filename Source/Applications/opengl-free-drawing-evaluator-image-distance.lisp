(defparameter *opengl-evaluator-pane* nil)


(defclass opengl-free-drawing-evaluator-image-distance (opengl-free-drawing-evaluator)
  ((pixels :initarg :pixels :accessor pixels)
   (image-pixels :initarg :image-pixels :accessor image-pixels)
   (image-path :initarg :image-path :accessor image-path)))


(defmethod initialize-properties :after ((o opengl-free-drawing-evaluator-image-distance))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'image-path :label "Image path" :accessor-type 'accessor-accessor-type :default-value "d:\\test.bmp" 
    :data-type 'string :editor 'text-editor)))

(defmethod objective-class ((evaluator opengl-free-drawing-evaluator-image-distance))
  'entity-opengl-free-drawing-2d)

(defmethod evaluate-opengl-free-drawing ((evaluator opengl-free-drawing-evaluator-image-distance) object)
  "Evaluation method for OpenGL free drawing object."
  (render-object-in-buffer evaluator object)
  (let ((distance (calculate-image-distance evaluator object)))
    (setf (fitness object) distance)
    distance))

;;; #TODO: Move gl-vector to fitness evaluator
(defun render-object-in-buffer (evaluator object)
  ;; #TODO: Determine which interface to use here
  (let ((interface (interface (interface-for-opengl-evaluator)))
        (pixels (opengl:make-gl-vector :unsigned-8 65536 #| :contents :length |#)))
    (redisplay-canvas (graphic-part interface))
    (opengl:gl-read-pixels 0 0 256 256 opengl:*gl-rgb* opengl:*gl-unsigned-byte* pixels)
    (setf (pixels evaluator) pixels)))

