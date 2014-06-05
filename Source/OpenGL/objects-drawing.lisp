
(defmethod draw-opengl-on ((o object-in-search) canvas viewer)
  "Draws OpenGL scene <o> on <canvas>."
  (draw-opengl-on (object o) canvas viewer))

(defun initialize-ortho-2d (a b c d)
  (opengl:gl-disable opengl:*GL-LIGHTING*)
  (opengl:gl-disable opengl:*GL-CULL-FACE*)
  (opengl:gl-shade-model opengl:*GL-SMOOTH*)
  (opengl:gl-matrix-mode opengl:*GL-PROJECTION*)
  (opengl:gl-load-identity)
  (opengl:gl-ortho (coerce a 'double-float) 
                   (coerce b 'double-float) 
                   (coerce c 'double-float) 
                   (coerce d 'double-float)
                   -1.d0 
                   1.d0)
  (opengl:gl-matrix-mode opengl:*GL-MODELVIEW*)
  (opengl:gl-load-identity))

#|
;; #TODO: Refactor into something like a visualization strategy
;; #NOTE: Height map clamped to [0, 1]
(defmethod draw-opengl-on ((o entity-function-x-y) canvas viewer)
  (let* ((width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (expresion (compiled-program o))
         (ix (/ width 10))
         (iy (/ height 10)))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d 0.d0 width height 0.d0)
      ;; Dibujado
      (dotimes (j height)
        (opengl:gl-begin opengl:*gl-quad-strip*)
        (dotimes (i width)
          (let ((x (/ i ix))
                (y (/ j iy))
                (value))
            (declare (special x) (special y))
            (setf value (handler-case
                            (coerce (crop-0-1 (funcall expresion)) 'single-float)
                          (error (function) 0.0)))
            (opengl:gl-color3-f value value value)
            (opengl:gl-vertex2-f (coerce i 'single-float) (coerce j 'single-float))
            (setf value (handler-case
                            (coerce (crop-0-1 (funcall expresion)) 'single-float)
                          (error (function) 0.0)))
            (opengl:gl-color3-f value value value)
            (opengl:gl-vertex2-f (coerce i 'single-float) (coerce (+ 1 j) 'single-float))))
        (opengl:gl-end)))
    (opengl:swap-buffers canvas)))
|#

;; Uses the minimum between 128 and the pixels
(defmethod draw-opengl-on ((o entity-function-x-y) canvas viewer)
  (let* ((steps (configuration-get image-steps))
         (width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (expresion (compiled-program o))
         (ix (/ width 10))
         (iy (/ height 10))
         (cx (min width steps))
         (cy (min height steps))
         (dx (/ width cx))
         (dy (/ height cy)))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d 0.d0 width height 0.d0)
      (dotimes (j (1+ cy))
        (opengl:gl-begin opengl:*gl-quad-strip*)
        (dotimes (i (1+ cx))
          (let ((x (/ (* i dx) ix))
                (y (/ (* j dy) iy))
                (value))
            (declare (special x) (special y))
            ;; #TODO: Check if use (handler-case (funcall expresion) (error (function) 0.0))
            (setf value (coerce (crop-0-1 (funcall expresion)) 'single-float))
            (opengl:gl-color3-f value value value)
            (opengl:gl-vertex2-f (coerce (* dx i) 'single-float) (coerce (* dy j) 'single-float))
            (setf y (/ (* (1+ j) dy) iy))
            ;; #TODO: Check if use (handler-case (funcall expresion) (error (function) 0.0))
            (setf value (coerce (crop-0-1 (funcall expresion)) 'single-float))
            (opengl:gl-color3-f value value value)
            (opengl:gl-vertex2-f (coerce (* dx i) 'single-float) (coerce (* dy (+ 1 j)) 'single-float))))
        (opengl:gl-end)))
  (opengl:swap-buffers canvas)))

(defmethod draw-opengl-on ((o entity-function-x) canvas viewer)
  "Compute pixmap values into <o> pixmap."
  (let* ((evaluator (fitness-evaluator (context (model (pane viewer)))))
         (expression (compiled-program o))
         (xmin (samples-xmin evaluator))
         (xmax (samples-xmax evaluator))
         (ymin (samples-ymin evaluator))
         (ymax (samples-ymax evaluator))
         (width (- xmax xmin))
         (height (- ymax ymin))
         (points)
         (x 0))
    (declare (special x))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d xmin (- xmax xmin) (- ymax ymin) ymin)
      (opengl:gl-clear opengl:*GL-COLOR-BUFFER-BIT*)
      ;; Draw object points
      (initialize-ortho-2d 0.0 (coerce width 'single-float) (coerce height 'single-float) 0.0)
      (opengl:gl-color3-f 1.0 1.0 1.0)
      (when (fitness-vector evaluator)
        (dotimes (i (samples evaluator))
          (setf x (aref (fitness-vector evaluator) i 0))
          (appendf points (list (list x (funcall expression)))))
        (draw-lines-with-points-gl points ymax))
      (opengl:swap-buffers canvas))))

;; Min between 128 and pixels count
(defmethod draw-opengl-on ((o entity-image-rgb) canvas viewer)
  (let* ((steps (configuration-get rgb-image-steps))
         (width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (expresion (compiled-program o))
         (ix width)
         (iy height)
         (cx steps)
         (cy steps)
         (dx (/ width cx))
         (dy (/ height cy)))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d 0.d0 width height 0.d0)
      ;; Draw
      (dotimes (j (1+ cy))
        (opengl:gl-begin opengl:*gl-quad-strip*)
        (dotimes (i (1+ cx))
          (let ((x (/ (* i dx) ix))
                (y (/ (* j dy) iy))
                (red) 
                (green)
                (blue))
            ;; Higher point
            (declare (special x) (special y))
            (let ((pixel (funcall expresion)))
              (if (numberp pixel) 
                  (let ((value (coerce (crop-0-1 pixel) 'single-float)))  
                    (setf red value green value blue value))
                (setf red (coerce (crop-0-1 (x pixel)) 'single-float) 
                      green (coerce (crop-0-1 (y pixel)) 'single-float)
                      blue (coerce (crop-0-1 (z pixel)) 'single-float)))
              (opengl:gl-color3-f red green blue)
              (opengl:gl-vertex2-f (coerce (* dx i) 'single-float) (coerce (* dy j) 'single-float)))
            ;; Lower point
            (setf y (/ (* (1+ j) dy) iy))
            (let ((pixel (funcall expresion)))
              (if (numberp pixel) 
                  (let ((value (coerce (crop-0-1 pixel) 'single-float)))  
                    (setf red value green value blue value))
                (setf red (coerce (crop-0-1 (x pixel)) 'single-float) 
                      green (coerce (crop-0-1 (y pixel)) 'single-float)
                      blue (coerce (crop-0-1 (z pixel)) 'single-float)))
              (opengl:gl-color3-f red green blue)
              (opengl:gl-vertex2-f (coerce (* dx i) 'single-float) (coerce (* dy (+ 1 j)) 'single-float)))))
        (opengl:gl-end)))
  (opengl:swap-buffers canvas)))

(defmethod draw-opengl-on ((o entity-sample-vrp) canvas viewer)
  (let* ((evaluator (fitness-evaluator (context (model (pane viewer)))))
         (width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (cities-description (cities-description evaluator))
         (depot-description (first cities-description))
         (depot-x (first depot-description))
         (depot-y (second depot-description))
         (route-index 0))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d (minimum-vrp-x o evaluator) width height (minimum-vrp-y o evaluator))
      (dolist (route (program o))
        (let ((route-color (route-color-for-index route-index)))
          ;; Draw route
          (opengl:gl-begin opengl:*GL-LINE-STRIP*)
          (opengl:gl-color3-f (aref route-color 1) (aref route-color 2) (aref route-color 3))
          (opengl:gl-vertex2-f (coerce depot-x 'single-float) (coerce depot-y 'single-float))
          (dolist (city route)
            (let ((city-x (first (nth city cities-description)))
                  (city-y (second (nth city cities-description))))
              (opengl:gl-vertex2-f (coerce city-x 'single-float) (coerce city-y 'single-float))))
          ;; Draw return to depot
          (opengl:gl-vertex2-f (coerce depot-x 'single-float) (coerce depot-y 'single-float))
          (opengl:gl-end)
          ;; Draw city points
          (opengl:gl-point-size (coerce 3 'single-float))
          (opengl:gl-begin opengl:*GL-POINTS*)
          (dolist (city route)
            (opengl:gl-vertex2-f 
             (coerce (first (nth city cities-description)) 'single-float) 
             (coerce (second (nth city cities-description)) 'single-float)))
          (opengl:gl-end))
        (incf route-index)))
    (opengl:swap-buffers canvas)))

(defmethod draw-opengl-on ((o graphic-function-r-r) canvas viewer)
  "Draws OpenGL scene <o> on <canvas>."
  (let* ((compiled-valuable (compiled-valuable o))
         (points (draw-data-gl o))
         (xmin (xmin o))
         (xmax (xmax o))
         (ymin (ymin o))
         (ymax (ymax o))
         (width (- xmax xmin))
         (height (- ymax ymin))
         (xmed (coerce (/ height 2) 'double-float))
         (x 0) 
         (y 0)
         (pane (pane (capi:element-interface canvas))))
    (declare (special x) (special y))
    (opengl:rendering-on (canvas)
      ;; Points
      (initialize-ortho-2d xmin (- xmax xmin) (- ymax ymin) ymin)
      (opengl:gl-clear opengl:*GL-COLOR-BUFFER-BIT*)
      (opengl:gl-color3-f 1.0 1.0 1.0)
      (draw-lines-gl points ymax)
      ;; Fonts
      (let* ((width 50)
             (height 15)
             (xmed (coerce (/ height 2) 'double-float)))
        (initialize-ortho-2d 0.0 (coerce width 'single-float) (coerce height 'single-float) 0.0)
        (ensure-set-up-gl-fonts canvas)
        (opengl:gl-color3-f 1.0 0.0 0.0)
        (draw-positioned-3d-text pane
                                 (format nil "~A" xmin) 
                                 0d0 xmed 0d0 0d0 180d0 0d0 1d0)
        (draw-positioned-3d-text pane
                                 (format nil "~A" xmax)
                                 (coerce (- width (* 4 (length (format nil "~A" xmax)))) 'double-float)
                                 xmed 0d0 0d0 180d0 0d0 1d0)
        (draw-positioned-3d-text pane
                                 (format nil "~A" ymin) 
                                 0d0 
                                 (coerce (- height 0.2) 'double-float) 
                                 0d0 0d0 180d0 0d0 1d0)
        (draw-positioned-3d-text pane
                                 (format nil "~A" ymax) 
                                 0d0 
                                 1d0
                                 0d0 0d0 180d0 0d0 1d0)
        (opengl:swap-buffers canvas)))))

(defun draw-lines-with-points-gl (point-list ymax)
  "Draws points from point-list."
  (draw-lines-gl point-list ymax)
  (draw-points-gl point-list ymax))

(defun draw-lines-gl (point-list ymax)
  "Draws points from point-list."
  (opengl:gl-point-size (coerce 1 'single-float))
  (opengl:gl-begin opengl:*GL-LINE-STRIP*)
  (draw-from-list point-list ymax)
  (opengl:gl-end))

(defun draw-points-gl (point-list ymax)
  "Draws points from <point-list>."
  (opengl:gl-point-size (coerce 5 'single-float))
  (opengl:gl-begin opengl:*GL-POINTS*)
  (draw-from-list point-list ymax)
  (opengl:gl-end))

(defun draw-from-list (point-list ymax)
  (dolist (i point-list)
    (opengl:gl-vertex2-f 
     (coerce (car i) 'single-float) 
     (coerce (- ymax (cadr i)) 'single-float))))

(defmethod draw-data-gl ((g graphic-function-r-r))
  "Answer the data to be drawn from <g>."
  (let* ((object (subject g))
         (points)
         (valuable-x (valuable-x g))
         (valuable-y (valuable-y g))
         (data (funcall (datasource g) object)))
    (dolist (i data)
      (push (list (funcall valuable-x i)
                  (funcall valuable-y i))
            points))
    points))

(defmethod draw-opengl-on ((o graphic-property-map) canvas viewer)
  "Compute pixmap values into <o> pixmap."
  (let* ((population-size (population-size (subject o)))
         (size-sqrt (ceiling (sqrt population-size)))
         (index 0))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d 0 size-sqrt size-sqrt 0)
      (opengl:gl-color3-f (coerce 1 'single-float) (coerce 1 'single-float) (coerce 1 'single-float))
      (dotimes (i size-sqrt)
        (dotimes (j size-sqrt)
          (if (< index population-size)
              (let* ((individual (aref (individuals-array (subject o)) index))
                     (value (get-value-for-property-named individual (property o)))
                     (value-float (coerce (/ (- value (value-min o))
                                             (- (value-max o) (value-min o)))
                                          'single-float)))
                (opengl:gl-color3-f value-float value-float value-float)
                (opengl:gl-begin opengl:*GL-QUADS*)
                (opengl:gl-vertex2-i i j)
                (opengl:gl-vertex2-i i (1+ j))
                (opengl:gl-vertex2-i (1+ i) (1+ j))
                (opengl:gl-vertex2-i (1+ i) j)
                (opengl:gl-end))
            (progn 
              (opengl:gl-color3-f 0.5 0.0 0.5)
              (opengl:gl-begin opengl:*GL-QUADS*)
              (opengl:gl-vertex2-i i j)
              (opengl:gl-vertex2-i i (1+ j))
              (opengl:gl-vertex2-i (1+ i) (1+ j))
              (opengl:gl-vertex2-i (1+ i) j)
              (opengl:gl-end)))
          (incf index)))
      (opengl:swap-buffers canvas))))

(defmethod draw-opengl-on ((o t) canvas viewer)
  "Compute pixmap values into <o> pixmap."
  (opengl:rendering-on (canvas)
    (initialize-ortho-2d 0 10 10 0)
    (opengl:gl-clear-color 0.0 0.0 0.0 1.0)
    (opengl:swap-buffers canvas)))

;; #TODO: Check if use (handler-case (funcall expresion) (error (function) 0.0))
(defmethod draw-opengl-on ((o entity-texture-deformation) canvas viewer)
  (let* ((steps (configuration-get texture-deformation-steps))
         (width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (expresion (compiled-program o))
         (cx steps)
         (cy steps)
         (dx (/ 1 cx))
         (dy (/ 1 cy)))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d 0.d0 1 1 0.d0)      
      (opengl:gl-color4-f 1.0 1.0 1.0 1.0)
      (turn-on-texture *texture-manager* (texture o))
      (dotimes (j (1+ cy))
        (opengl:gl-begin opengl:*gl-quad-strip*)
        (dotimes (i (1+ cx))
          (let ((x (* i dx))
                (y (* j dy))
                (texture-coord-x)
                (texture-coord-y))
            (declare (special x) (special y))
            (multiple-value-bind (x-part y-part)
                (funcall expresion)
              (setf texture-coord-x (coerce (crop-0-1 x-part) 'single-float)
                    texture-coord-y (coerce (crop-0-1 y-part) 'single-float))
              (opengl:gl-tex-coord2-f texture-coord-x texture-coord-y)
              (opengl:gl-vertex2-f (coerce (* dx i) 'single-float) (coerce (* dy j) 'single-float)))
            (setf y (* (1+ j) dy))
            (multiple-value-bind (x-part y-part)
                (funcall expresion)
              (setf texture-coord-x (coerce (crop-0-1 x-part) 'single-float)
                    texture-coord-y (coerce (crop-0-1 y-part) 'single-float))
              (opengl:gl-tex-coord2-f texture-coord-x texture-coord-y)
              (opengl:gl-vertex2-f (coerce (* dx i) 'single-float) (coerce (* (1+ j) dy) 'single-float)))))
        (opengl:gl-end)))
    (opengl:swap-buffers canvas)))

#|
(defmethod draw-opengl-on ((o entity-texture-deformation) canvas viewer)
  (let ((width (slot-value canvas 'graphics-ports::width))
        (height (slot-value canvas 'graphics-ports::height))
        (expresion (compiled-program o)))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d 0.d0 1 1 0.d0)
      (opengl:gl-color4-f 1.0 1.0 1.0 1.0)
      (turn-on-texture *texture-manager* 'sample)
      (opengl:gl-begin opengl:*gl-quads*)
      (opengl:gl-tex-coord2-f (coerce 0 'single-float) (coerce 0 'single-float))
      (opengl:gl-vertex2-f (coerce 0 'single-float) (coerce 0 'single-float))
      (opengl:gl-tex-coord2-f (coerce 1 'single-float) (coerce 0 'single-float))
      (opengl:gl-vertex2-f (coerce 1 'single-float) (coerce 0 'single-float))
      (opengl:gl-tex-coord2-f (coerce 1 'single-float) (coerce 1 'single-float))
      (opengl:gl-vertex2-f (coerce 1 'single-float) (coerce 1 'single-float))
      (opengl:gl-tex-coord2-f (coerce 0 'single-float) (coerce 1 'single-float))
      (opengl:gl-vertex2-f (coerce 0 'single-float) (coerce 1 'single-float))
      (opengl:gl-end)
      (opengl:swap-buffers canvas))))
|#