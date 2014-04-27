(defclass entity-image-bw (entity-function-x-y)
  ())


(defmethod equivalent ((a entity-image-bw) (b entity-image-bw) 
                        &optional &key check-genotype check-phenotype criteria)
  "Answer whether <a> and <b> are equivalent."
  (declare (ignore criteria))
  (or (and check-genotype (equal (program a) (program b)))
      (and check-phenotype (compare-image a b))))

(defmethod compare-image ((a entity-image-bw) (b entity-image-bw))
  "Answer whether <a> and <b> are the same, for a defined set of points."
  (let* ((pixels-x (pixels-x a))
         (pixels-y (pixels-y a))
         (delta-x (/ (heigth a) pixels-x))
         (delta-y (/ (width a) pixels-y))
         (function-a (compiled-program a))
         (function-b (compiled-program b))
         (x)
         (y)
         (start-x (start-position-x a))
         (start-y (start-position-y a)))
    (declare (special x) 
             (special y))
    (block 1
      (dotimes (i pixels-x)
        (dotimes (j pixels-y)
          (setf x (+ start-x (* i delta-x))
                y (+ start-y (* j delta-y)))
          (if (not (equals (vec-crop 0 1 (funcall function-a))
                           (vec-crop 0 1 (funcall function-b))))
              (return-from 1 nil))))
      t)))

(defmethod constant-p ((o entity-image-bw) &optional (check-genotype t) (check-phenotype t))
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
               (first-value)
               (x)
               (y)
               (start-position-x (start-position-x o))
               (start-position-y (start-position-y o)))
          (declare (special x) (special y))
          (block 1
            (dotimes (i pixels-x)
              (dotimes (j pixels-y)
                (setf x (+ start-position-x (* i delta-x))
                      y (+ start-position-y (* j delta-y)))
                (if first-value
                    (if (not (equals (vec-crop 0 1 (funcall function)) first-value))
                        (return-from 1 nil))
                  (setf first-value (vec-crop 0 1 (funcall function))))))
            t))
      nil)))      