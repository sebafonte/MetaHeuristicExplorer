
(defclass entity-function (entity)
  ())


(defmethod ephemeral-properties-definition ((o entity-function))
  "Answer <o> ephemeral properties."
  (property-from-values-list
   o
   (:name 'program :label "Program" :accessor-type 'valuable-accessor-type 
    :data-type 'list :editor 'list-editor :read-only t :getter 'program-text)
   (:name 'size :label "Size" :accessor-type 'valuable-accessor-type 
    :data-type 'integer :read-only t :editor 'number-editor :getter 'structure-size)
   (:name 'fitness :label "Fitness" :accessor-type 'accessor-accessor-type :read-only t
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 10000 :editor 'number-editor)))

(defmethod compute-interface-pixmap ((pixmap t) (pinboard t) (pane t) (o object-in-search) width height)
  "Compute pixmap values into pixmap of <o>."
  (if (not (image-buffer pane))
      (setf (image-buffer pane) 
            (if (= (image-render-step pane) 1)
                (compute-object-interface-pixmap-normal
                 (object o) (context o) pixmap width height)
              (compute-object-interface-pixmap-step
                 (object o) (context o) pixmap width height (image-render-step pane)))))
  (gp:draw-image pixmap (image-buffer pane) 0 0))

(defmethod compute-object-interface-pixmap-normal 
           ((o entity-function) subtask pixmap width height)
  "Compute pixmap values into pixmap of <o>."
  (let* ((evaluator (fitness-evaluator subtask))
         (expresion (compiled-program o))
         (measure-start (measure-start evaluator))
         (measure-end (measure-end evaluator))
         (ajuste-x (/ width (- measure-end measure-start)))
         (ajuste-y (/ height (- measure-end measure-start)))
         (bgra-vector (make-array (* height width 4) :element-type '(unsigned-byte 8)))
         (bgra (make-array (list height width 4) :element-type '(unsigned-byte 8) :displaced-to bgra-vector))
         (image  (gp:make-image-from-port pixmap 0 0 width height))
         (access (gp:make-image-access pixmap image))
         (x 0) 
         (y 0)
         (value))
    (declare (special x) (special y)
             (number x) (number y) (number ix) (number iy) (number ajuste-x) (number ajuste-y)
             (number measure-start) (number measure-end))
    (dotimes (ix width)
      (setf x (/ ix ajuste-x))
      (dotimes (iy height)
        (setf y (/ iy ajuste-y)
              value (handler-case
                      (coerce (round (* (crop-0-1 (funcall expresion)) 255)) 'unsigned-byte)
                      (error (function) 0))
              (aref bgra iy ix 0) value
              (aref bgra iy ix 1) value
              (aref bgra iy ix 2) value
              (aref bgra iy ix 3) 255)))
    (gp:image-access-pixels-from-bgra access bgra-vector)
    (gp:free-image-access access)
    image))

(defmethod compute-object-interface-pixmap-step 
           ((o entity-function) subtask pixmap width height render-precision)
  "Compute pixmap values into pixmap of <o>."
  (let* ((evaluator (fitness-evaluator subtask))
         (expresion (compiled-program o))
         (measure-start (measure-start evaluator))
         (measure-end (measure-end evaluator))
         (ajuste-x (/ width (- measure-end measure-start)))
         (ajuste-y (/ height (- measure-end measure-start)))
         (bgra-vector (make-array (* height width 4) :element-type '(unsigned-byte 8)))
         (bgra (make-array (list height width 4) :element-type '(unsigned-byte 8) :displaced-to bgra-vector))
         (image  (gp:make-image-from-port pixmap 0 0 width height))
         (access (gp:make-image-access pixmap image))
         (x 0) 
         (y 0)
         (value)
         (ixs (floor (/ width render-precision)))
         (iys (floor (/ height render-precision))))
    (declare (special x) (special y) (render-precision integer)
             (number x) (number y) (number ix) (number iy) (number ajuste-x) (number ajuste-y)
             (number measure-start) (number measure-end))
    (dotimes (i ixs)
      (setf x (* render-precision (/ i ajuste-x)))
      (dotimes (j iys)
        (setf y (* render-precision (/ j ajuste-y))
              value (handler-case
                        (coerce (round (* (crop-0-1 (funcall expresion)) 255)) 'unsigned-byte)
                      (error (function) 0)))
        (dotimes (ix render-precision)
          (let ((xs (+ (* render-precision i) ix)))
            (if (< xs width)
                (dotimes (iy render-precision)
                  (let ((ys (+ (* render-precision j) iy)))
                    (if (< ys height)
                        (setf (aref bgra ys xs 0) value
                              (aref bgra ys xs 1) value
                              (aref bgra ys xs 2) value
                              (aref bgra ys xs 3) 255)))))))))
    (gp:image-access-pixels-from-bgra access bgra-vector)
    (gp:free-image-access access)
    image))

(defmethod default-population-initializer ((o entity-function))
  "Answer the default population initializer for <o>."
  (system-get 'random-trees-initializer))

(defmethod published-actions ((o entity-function))
  "Answer the published actions for <o>."
  (list (list "Add as system function" 'add-system-function)))

(defun add-system-function (entity-function)
  "Adds <o> as a system function."
  nil)
