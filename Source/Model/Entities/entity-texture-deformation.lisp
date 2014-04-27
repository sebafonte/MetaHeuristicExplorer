(defclass entity-texture-deformation (entity-image-bw object-with-properties)
  ((texture :initarg :texture :accessor texture)))


(defmethod initialize-properties ((o entity-texture-deformation))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'texture :label "Texture" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'texture-editor :default-value 'sample7)))

(defmethod abstractp ((o (eql 'entity-texture-deformation)))
  "Answer whether <o> is abstract."
  t)

(defmethod draw-in-pixmap (pinboard object pane (o entity-texture-deformation) parent-pinboard x y)
  "Draws <object> in the pixmap of <pinboard> interface."
  (not-available-pixmap pinboard object pane o parent-pinboard x y))

(defmethod not-available-pixmap (pinboard object pane object parent-pinboard x y)
  "Compute a not avaiable image for <object>."
  nil)

;; #TODO: Check if use (handler-case (funcall expresion) (error (function) 0.0))
(defmethod draw-opengl-on ((o entity-texture-deformation) canvas viewer)
  "Draws OpenGL scene <o> on <canvas>."
  (let* ((width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (expresion (compiled-program o))
         (ix (/ width 10))
         (iy (/ height 10))
         (cx (min width 100))
         (cy (min height 100))
         (dx (/ width cx))
         (dy (/ height cy)))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d 0.d0 width height 0.d0)      
      (dotimes (j (1+ cy))
        (opengl:gl-begin opengl:*gl-quad-strip*)
        (dotimes (i (1+ cx))
          (let ((x (/ (* i dx) ix))
                (y (/ (* j dy) iy))
                (texture-coord-x)
                (texture-coord-y))
            (declare (special x) (special y))
            (multiple-value-bind (x-part y-part)
                (funcall expresion)
              (setf texture-coord-x (coerce (crop-0-1 x-part) 'single-float)
                    texture-coord-y (coerce (crop-0-1 y-part) 'single-float))
              (opengl:gl-tex-coord2-f texture-coord-x texture-coord-y)
              (opengl:gl-vertex2-f (coerce (* dx i) 'single-float) (coerce (* dy j) 'single-float)))
            (setf y (/ (* (1+ j) dy) iy))
            (multiple-value-bind (x-part y-part)
                (funcall expresion)
              (setf texture-coord-x (coerce (crop-0-1 x-part) 'single-float)
                    texture-coord-y (coerce (crop-0-1 y-part) 'single-float))
              (opengl:gl-tex-coord2-f texture-coord-x texture-coord-y)
              (opengl:gl-vertex2-f (coerce (* dx i) 'single-float) (coerce (* dy j) 'single-float)))))
        (opengl:gl-end)))
  (handler-case (opengl:swap-buffers canvas)
    (error (function) nil))))

(defmethod default-fitness-evaluators ((object entity-texture-deformation))
  "Answer the default classes that can evaluate object fitness."
  (list 
   (system-get 'entity-rgb-evaluator)
   (system-get 'entity-seamless-basic)))



