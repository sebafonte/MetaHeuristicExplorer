(defclass entity-image-rgb-evaluator (entity-image-bw-evaluator)
  ((constant-check :initarg :constant-check :accessor constant-check)
   (constant-x :initarg :constant-check :accessor constant-x)
   (constant-y :initarg :constant-check :accessor constant-y)))


(defmethod possible-fitness-functions ((o entity-image-rgb-evaluator))
  "Answer <o> possible fitness functions."
  '(evaluate-no-evaluation
    evaluate-single-color-penalty
    evaluate-squared-distance
    evaluate-distance
    evaluate-distance-ponderated))

(defmethod evaluate-distance ((evaluator entity-image-rgb-evaluator) (object entity-image-rgb))
  "Evaluate using absolute difference with target values."
  (let* ((function (compiled-program object)) 
         (fitness-vector (fitness-vector evaluator))
         (samples (samples evaluator))
         (result 0) 
         (error 0)
         (x 0)
         (y 0))
    (declare (special x) (special y)
             (number x) (number y) (number samples) (number error) (number result))
    (dotimes (i samples)
      (dotimes (j samples)
        (setf result (aref fitness-vector i j)
              x (car result)
              y (cadr result))
        (incf error (vec-abs (vec-- (caddr result) (funcall function))))))
    (setf (fitness object) 
          (/ 10 (1+ (/ error (* samples samples)))))))

(defmethod evaluate-squared-distance ((evaluator entity-image-rgb-evaluator) 
                                      (object entity-image-rgb))
  "Evaluate using squared difference with target values."
  (let* ((function (compiled-program object))
         (samples (samples evaluator))
         (fitness-vector (fitness-vector evaluator))
         (result 0) 
         (error 0)
         (x 0)
         (y 0))
    (declare (special x) (special y)
             (number x) (number y) (number samples) (number error) (number result))
    (dotimes (i samples)
      (dotimes (j samples)
        (setf result (aref fitness-vector i j)
              x (car result)
              y (cadr result))
        (incf error (sqr (vec-abs (vec-- (caddr result) (funcall function)))))))
    (setf (fitness object) 
          (/ 10 (sqr (1+ (/ error (* samples samples))))))))

(defmethod measure-start ((evaluator entity-image-rgb-evaluator))
  "Answer the start measure value for evaluator."
  0)

(defmethod measure-end ((evaluator entity-image-rgb-evaluator))
  "Answer the end measure value for evaluator."
  1)



