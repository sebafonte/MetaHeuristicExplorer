(defclass entity-image-seamless-evaluator (entity-image-bw-evaluator)
  ())


(defmethod evaluar-imagen-seamless ((evaluator entity-image-seamless-evaluator) 
                                    (o entity-image-bw))
  "Answer <o> fitness.
   #CHECK: Consider a similarity level."
  (let* ((pixels-x (pixels-x o))
         (pixels-y (pixels-y o))
         (heigth (heigth o))
         (width (width o))
         (dx (/ heigth pixels-x))
         (dy (/ width pixels-y))
         (difference-x 0)
         (difference-y 0)
         (function (compiled-program o))
         (start-x (start-position-x o))
         (start-y (start-position-y o))
         (initial-x (+ start-x dx))
         (final-x (+ start-x (- width dx)))
         (initial-y (+ start-y dy))
         (final-y (+ start-y (- heigth dy))))
    ;; Increment differences over x axis
    (dotimes (i pixels-x)
      (incf difference-x (point-difference o function initial-x (* i dy) final-x (* i dy))))
    ;; Increment differences over y axis
    (dotimes (i pixels-y)
      (incf difference-y (point-difference o function (* i dx) initial-y (* i dx) final-y)))
    ;; Fitness weight and setf
    (setf (fitness o) (weigth-evaluation-2 evaluator o difference-x difference-y))))

(defmethod weigth-evaluation-1 ((evaluator entity-image-seamless-evaluator) o difference-x difference-y)
  "Answer weighted fitness for <o>."
  (if (constant-p o) 
      0 
    (/ 10 (+ 1 difference-x difference-y))))

(defmethod weigth-evaluation-2 ((evaluator entity-image-seamless-evaluator) o difference-x difference-y)
  "Answer weighted fitness for <o>."
  (/ 10 (+ 1 difference-x difference-y)))

(defmethod point-difference ((o entity-function-x-y) function a b c d)
  "Answer difference value for a scalar <function> object over points <a,b> and <c,d>."
  (point-difference-x-y function a b c d))

(defun point-difference-x-y (function a b c d)
  "Answer difference value for a scalar <function> object over points <a,b> and <c,d>."
  (let ((x (point-value function a b)) 
        (y (point-value function c d)))
    (abs (- y x))))

(defun point-value (function x-value y-value)
  "Answer <function> value over bi-dimensional point <x-value, y-value>."
  (declare (special x) (special y))
  (setf x x-value y y-value)
  (funcall function))

(defmethod possible-fitness-functions ((o entity-image-seamless-evaluator))
  "Answer <o> possible fitness functions."
  '(evaluar-imagen-seamless))


#|
(defmethod constant-on-y ((o entity-image-seamless))
  "Answers whether <o> is constant along x axis."
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
        (setf x start-position-x
              y start-position-y
              first-value (funcall function))
        (dotimes (j pixels-y)
          (setf x (+ start-position-x (* i delta-x))
                y (+ start-position-y (* j delta-y)))
          (if (not (= (funcall function) first-value))
              (return-from 1 nil))))
      t))) 
|#
