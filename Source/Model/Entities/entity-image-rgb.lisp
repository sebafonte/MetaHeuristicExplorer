(defclass entity-image-rgb (entity-image-bw)
  ())


(defmethod lambda-default-fitness-comparer ((a entity-image-rgb) (b entity-image-rgb))
  "Answer whether <a> is better than <b>."
  (< (fitness a) (fitness b)))

(defmethod compute-object-interface-pixmap-normal ((o entity-image-rgb) subtask pixmap width heigth)
  "Compute pixmap values into <pixmap> of <o>."
  (let* ((evaluator (fitness-evaluator subtask))
         (program (compiled-program o))
         (measure-start (measure-start evaluator))
         (measure-end (measure-end evaluator))
         (ajuste-x (/ width (- measure-end measure-start)))
         (ajuste-y (/ heigth (- measure-end measure-start)))
         (x 0)
         (y 0)
         (bgra-vector (make-array (* heigth width 4) :element-type '(unsigned-byte 8)))
         (bgra (make-array (list heigth width 4) :element-type '(unsigned-byte 8) :displaced-to bgra-vector))
         (image  (gp:make-image-from-port pixmap 0 0 width heigth))
         (access (gp:make-image-access pixmap image)))
      (declare (special x) (special y) (number measure-start) (number measure-end)
               (number x) (number y) (number ix) (number iy) (number ajuste-x) (number ajuste-y))
      (dotimes (ix width)
        (setf x (/ ix ajuste-x))
        (dotimes (iy heigth)
          (setf y (/ iy ajuste-y)
                (aref bgra iy ix 3) 255)
          (let ((pixel (funcall program)))
            (if (numberp pixel) 
                (let ((value (coerce (round (* (crop-0-1 pixel) 255)) 'unsigned-byte)))  
                  (setf (aref bgra iy ix 0) value
                        (aref bgra iy ix 1) value
                        (aref bgra iy ix 2) value))
              (setf (aref bgra iy ix 0) (coerce (round (* (crop-0-1 (z pixel)) 255)) 'unsigned-byte) 
                    (aref bgra iy ix 1) (coerce (round (* (crop-0-1 (y pixel)) 255)) 'unsigned-byte)
                    (aref bgra iy ix 2) (coerce (round (* (crop-0-1 (x pixel)) 255)) 'unsigned-byte))))))
      (gp:image-access-pixels-from-bgra access bgra-vector)
      (gp:free-image-access access)
      image))

(defmethod compute-object-interface-pixmap-step ((o entity-image-rgb) subtask pixmap width heigth render-precision)
  "Compute pixmap values into <pixmap> of <o>."
  (let* ((evaluator (fitness-evaluator subtask))
         (program (compiled-program o))
         (measure-start (measure-start evaluator))
         (measure-end (measure-end evaluator))
         (ajuste-x (/ width (- measure-end measure-start)))
         (ajuste-y (/ heigth (- measure-end measure-start)))
         (x 0)
         (y 0)
         (bgra-vector (make-array (* heigth width 4) :element-type '(unsigned-byte 8)))
         (bgra (make-array (list heigth width 4) :element-type '(unsigned-byte 8) :displaced-to bgra-vector))
         (image  (gp:make-image-from-port pixmap 0 0 width heigth))
         (access (gp:make-image-access pixmap image))
         (ixs (ceiling (/ width render-precision)))
         (iys (ceiling (/ heigth render-precision))))
      (declare (special x) (special y) (number measure-start) (number measure-end)
               (number x) (number y) (number ix) (number iy) (number ajuste-x) (number ajuste-y))
      (dotimes (i ixs)
        (setf x (* render-precision (/ i ajuste-x)))
        (dotimes (j iys)
          (setf y (* render-precision (/ j ajuste-y)))
          (let ((pixel (funcall program)))
            (dotimes (ix render-precision)
              (let ((xs (+ (* render-precision i) ix)))
                (if (< xs width)
                    (dotimes (iy render-precision)
                      (let ((ys (+ (* render-precision j) iy)))
                        (if (< ys heigth)
                            (if (numberp pixel) 
                                (let ((value (coerce (round (* (crop-0-1 pixel) 255)) 'unsigned-byte)))  
                                  (setf (aref bgra ys xs 0) value
                                        (aref bgra ys xs 1) value
                                        (aref bgra ys xs 2) value
                                        (aref bgra ys xs 3) 255))
                            (setf (aref bgra ys xs 0) (coerce (round (* (crop-0-1 (z pixel)) 255)) 'unsigned-byte) 
                                  (aref bgra ys xs 1) (coerce (round (* (crop-0-1 (y pixel)) 255)) 'unsigned-byte)
                                  (aref bgra ys xs 2) (coerce (round (* (crop-0-1 (x pixel)) 255)) 'unsigned-byte)
                                  (aref bgra ys xs 3) 255)))))))))))
      (gp:image-access-pixels-from-bgra access bgra-vector)
      (gp:free-image-access access)
      image))

(defmethod default-fitness-evaluators ((object entity-image-rgb))
  "Answer the default classes that can evaluate object fitness."
  (list 
   (system-get 'entity-rgb-evaluator)
   (system-get 'entity-image-similarity-pixel-distance)))

(defmethod possible-languages ((o entity-image-rgb))
  (list 
   (system-get 'rgb-color-images-vector)
   (system-get 'rgb-color-images)))

#|
(defmethod constant-p ((o entity-image-rgb) &optional (check-genotype t) (check-phenotype t))
  "Answers whether <o> is a 1 colour constant image."
  (block 1
    ;; Check genotype
    (if (and check-genotype (subexp-constant-p (program o) o))
      (return-from 1 t))
    ;; Check phenotype
    (if check-phenotype
        (let* ((pixels-x (pixels-x o))
               (pixels-y (pixels-y o))
               (delta-x (/ (heigth o) pixels-x))
               (delta-y (/ (width o) pixels-y))
               (function (compiled-program o))
               (start-x (start-position-x o))
               (start-y (start-position-y o))
               (first-value)
               (x)
               (y))
          (declare (special x) (special y))
          (block 1
            (dotimes (i pixels-x)
              (dotimes (j pixels-y)
                (setf x (+ start-x (* i delta-x))
                      y (+ start-y (* j delta-y)))
                (if first-value
                    (if (not (equals (vec-crop 0 1 (funcall function)) first-value))
                        (return-from 1 nil))
                  (setf first-value (vec-crop 0 1 (funcall function))))))
            t))
      nil)))
|#