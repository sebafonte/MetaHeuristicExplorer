
(defclass entity-sample-vrp (entity)
  ())


(defmethod default-fitness-evaluators ((object entity-sample-vrp))
  "Answer the default classes that can evaluate <object> fitness."
  (list 
   (system-get 'sample-vrp-evaluator)
   (system-get 'default-vrp-evaluator)
   (system-get 'default-dvrp-evaluator)))

(defmethod default-population-initializer ((object entity-sample-vrp))
  "Answer the default population initializer class name of <object>."
  (system-get 'sample-vrp-initializer))

(defmethod compute-object-interface-pixmap-step
           ((o entity-sample-vrp) subtask pixmap width heigth render-precision)
  "Computes pixel values into <pixmap> of <o>."
  (declare (ignore render-precision))
  (let* ((bgra-vector (make-array (* heigth width 4) :element-type '(unsigned-byte 8)))
         (bgra (make-array (list heigth width 4) :element-type '(unsigned-byte 8) :displaced-to bgra-vector))
         (image (gp:make-image-from-port pixmap 0 0 width heigth))
         (access (gp:make-image-access pixmap image)))
    (draw-background-in-pixmap o subtask pixmap width heigth bgra)
    (draw-depot-in-pixmap o subtask pixmap width heigth bgra)
    (draw-cities-in-pixmap o subtask pixmap width heigth bgra)
    (gp:image-access-pixels-from-bgra access bgra-vector)
    (gp:free-image-access access)
    image))

(defmethod post-process-interface-pixmap-subtask ((object entity-sample-vrp) subtask o width height)
  "Draw final details of <object> on <pixmap>."
  (draw-routes-in-pixmap object subtask o width height))

(defmethod draw-background-in-pixmap ((o entity-sample-vrp) subtask pixmap width heigth bgra)
  "Draw background for <o> and <subtask> in <pixmap>."
  (dotimes (x width)
    (dotimes (y heigth)
      (setf (aref bgra y x 0) 0
            (aref bgra y x 1) 0
            (aref bgra y x 2) 0
            (aref bgra y x 3) 255))))

(defun minimum-vrp-x (o evaluator)
  "Answer minimum x coordinate for <evaluator>."
  (declare (ignore o))
  (reduce 'min (mapcar (lambda (x) (car x)) 
                      (cities-description evaluator))))

(defun minimum-vrp-y (o evaluator)
  "Answer minimum y coordinate for <evaluator>."
  (declare (ignore o))
  (reduce 'min (mapcar (lambda (x) (cadr x)) 
                       (cities-description evaluator))))

(defun maximum-vrp-x (o evaluator)
  "Answer maximum x coordinate for <evaluator>."
  (declare (ignore o))
  (reduce 'max (mapcar (lambda (x) (car x)) 
                       (cities-description evaluator))))

(defun maximum-vrp-y (o evaluator)
  "Answer maximum y coordinate for <evaluator>."
  (declare (ignore o))
  (reduce 'max (mapcar (lambda (x) (cadr x)) 
                       (cities-description evaluator))))

(defun route-color-for-index (index)
  "Answer a rgb-color instance for <index>."
  (cond 
   ((= index 0) (color:make-rgb 1.0 1.0 1.0))
   ((= index 1) (color:make-rgb 1.0 0.0 0.0))
   ((= index 2) (color:make-rgb 0.0 1.0 0.0))
   ((= index 3) (color:make-rgb 0.0 0.0 1.0))
   ((= index 4) (color:make-rgb 1.0 1.0 0.0))
   ((= index 5) (color:make-rgb 0.0 1.0 1.0))
   ((= index 6) (color:make-rgb 1.0 0.0 1.0))
   (t (color:make-rgb 0.5 0.5 0.5))))
 
(defmethod draw-depot-in-pixmap ((o entity-sample-vrp) subtask pixmap width heigth bgra)
  "Draw depot for <o> in <pixmap>."
  (let* ((evaluator (fitness-evaluator subtask))
         (x-factor (* (/ width (- (maximum-vrp-x o evaluator) (minimum-vrp-x o evaluator))) 0.9))
         (y-factor (* (/ heigth (- (maximum-vrp-y o evaluator) (minimum-vrp-y o evaluator))) 0.9))
         (cities-description (cities-description evaluator))
         (depot-description (first cities-description))
         (depot-x (first depot-description))
         (depot-y (second depot-description))
         (center-x 5)
         (center-y 5)
         (x (* x-factor (- depot-x (minimum-vrp-x o evaluator))))
         (y (* y-factor (- depot-y (minimum-vrp-y o evaluator))))
         (depot-x-pos (floor (+ center-x x)))
         (depot-y-pos (floor (+ center-y y))))
    (draw-centered-quad depot-x-pos depot-y-pos bgra 0 0 255 8)))

(defmethod lambda-default-fitness-comparer ((a entity-sample-vrp) (b entity-sample-vrp))
  (< (fitness a) (fitness b)))

(defmethod lambda-default-fitness-value-comparer ((a entity-sample-vrp) fitness-value)
  (< (fitness a) fitness-value))

(defmethod drawablep ((o entity-sample-vrp))
  "Answer whether <o> can be displayed on the GUI."
  t)

(defmethod prepare-children-from ((o entity-sample-vrp) children algorithm)
  "Prepares <o> to behave like <children>."
  (declare (ignore algorithm))
  (setf (program o) (program children)))

(defmethod possible-languages ((o entity-sample-vrp))
  (list 
   (system-get 'vrp-default-language)))


(defmethod draw-cities-in-pixmap ((o entity-sample-vrp) subtask pixmap width heigth bgra)
  "Draw cities description in <pixmap> for <subtask>."
  (declare (ignore pixmap))
  (let* ((center-x 5)
         (center-y 5)
         (evaluator (fitness-evaluator subtask))
         (x-factor (* (/ width (- (maximum-vrp-x o evaluator) (minimum-vrp-x o evaluator))) 0.9))
         (y-factor (* (/ heigth (- (maximum-vrp-y o evaluator) (minimum-vrp-y o evaluator))) 0.9)))
    (dolist (city (cdr (cities-description (fitness-evaluator subtask))))
      (let* ((city-x (first city))
             (city-y (second city))
             (x (* x-factor (- city-x (minimum-vrp-x o evaluator))))
             (y (* y-factor (- city-y (minimum-vrp-y o evaluator))))
             (x-pos (floor (+ center-x x)))
             (y-pos (floor (+ center-y y))))
        (draw-centered-quad x-pos y-pos bgra 0 255 0 4)))))

(defun draw-centered-quad (i j array r g b value)
  "Draw a centered quad around <i> and <j> positions with <value> thickness and <r, g, b> color."
  (dotimes (xi (ceiling (/ value 2)))
    (dotimes (yi (ceiling (/ value 2)))
      (let ((x (+ i xi))
            (y (+ j yi)))
        (setf (aref array y x 0) r
              (aref array y x 1) g
              (aref array y x 2) b
              (aref array y x 3) 255))
      (let ((x (- i xi))
            (y (+ j yi)))
        (setf (aref array y x 0) r
              (aref array y x 1) g
              (aref array y x 2) b
              (aref array y x 3) 255))
      (let ((x (+ i xi))
            (y (- j yi)))
        (setf (aref array y x 0) r
              (aref array y x 1) g
              (aref array y x 2) b
              (aref array y x 3) 255))
      (let ((x (- i xi))
            (y (- j yi)))
        (setf (aref array y x 0) r
              (aref array y x 1) g
              (aref array y x 2) b
              (aref array y x 3) 255)))))

(defmethod draw-routes-in-pixmap ((o entity-sample-vrp) subtask pixmap width heigth)
  "Draw routes for <o> in <pixmap>."
  (let* ((evaluator (fitness-evaluator subtask))
         (x-factor (* (/ width (- (maximum-vrp-x o evaluator) (minimum-vrp-x o evaluator))) 0.9))
         (y-factor (* (/ heigth (- (maximum-vrp-y o evaluator) (minimum-vrp-y o evaluator))) 0.9))
         (cities-description (cities-description evaluator))
         (depot-description (first cities-description))
         (route-index 0)
         (depot-x (first depot-description))
         (depot-y (second depot-description))
         (center-x 5)
         (center-y 5))
    (dolist (route (program o))
      (let* ((x (* x-factor (- depot-x (minimum-vrp-x o evaluator))))
             (y (* y-factor (- depot-y (minimum-vrp-y o evaluator))))
             (depot-x-pos (floor (+ center-x x)))
             (depot-y-pos (floor (+ center-y y)))
             (prev-x-pos depot-x-pos)
             (prev-y-pos depot-y-pos)
             (route-color (route-color-for-index route-index)))
        ;; Draw route
        (dolist (city route)
          (let* ((city-x (first (nth city cities-description)))
                 (city-y (second (nth city cities-description)))
                 (x (* x-factor (- city-x (minimum-vrp-x o evaluator))))
                 (y (* y-factor (- city-y (minimum-vrp-y o evaluator))))
                 (x-pos (floor (+ center-x x)))
                 (y-pos (floor (+ center-y y))))
            (gp:draw-lines (graphics-ports::pixmap-port-owner pixmap) 
                           (list prev-x-pos prev-y-pos x-pos y-pos)
                           :foreground route-color)
            (setf prev-x-pos x-pos
                  prev-y-pos y-pos)))
        ;; Draw return to depot
        (gp:draw-lines (graphics-ports::pixmap-port-owner pixmap) 
                       (list prev-x-pos prev-y-pos depot-x-pos depot-y-pos)
                       :foreground route-color))
      (incf route-index))))
