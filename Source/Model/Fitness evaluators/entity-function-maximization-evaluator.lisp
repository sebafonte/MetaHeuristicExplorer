
(defclass entity-function-maximization-evaluator (entity-evaluator)
  ((target-program :initarg :target-program :accessor target-program)
   (compiled-target-program :initarg :compiled-target-program :accessor compiled-target-program)
   (decode-function :initarg :decode-function :accessor decode-function)
   (measure-start :initarg :measure-start :accessor measure-start)
   (measure-end :initarg :measure-end :accessor measure-end)))


(defmethod initialize-properties :after ((o entity-function-maximization-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'target-program :label "Target program" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'lisp-editor :default-value '(* x x)
    :possible-values '((* x x)
                       (+ 1 (* x (sin (* 10 pi x))))))
   (:name 'decode-function :label "Decode function" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'decode-to-double :editor 'list-editor 
    :possible-values '(decode-to-double))
   (:name 'measure-start :label "Measure start" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 0 :editor 'number-editor)
   (:name 'measure-end :label "Measure end" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 10 :editor 'number-editor)))

(defmethod initialize-fitness-data ((e entity-function-maximization-evaluator))
  "Initialize <o> fitness data."
  (setf (compiled-target-program e) 
        (compile nil `(lambda (bit-value)
                        (let ((x (,(decode-function e) ,e bit-value)))
                          ,(target-program e))))))

(defmethod evaluate ((e entity-function-maximization-evaluator) (o entity-function-maximization))
  "Evaluate using absolute difference with target values."
  (setf (fitness o) (funcall (compiled-target-program e) (program o))))

(defmethod objective-class ((e entity-function-maximization-evaluator))
  'entity-function-maximization)

;; #TODO: Use target-program to implement these
(defmethod samples-xmin ((e entity-function-maximization-evaluator))
  0)

(defmethod samples-xmax ((e entity-function-maximization-evaluator))
  0)

(defmethod samples-ymin ((e entity-function-maximization-evaluator))
  0)

(defmethod samples-ymax ((e entity-function-maximization-evaluator))
  0)

;; #NOTE: Temporal function to test fitness
(defmethod decode-to-double ((e entity-function-maximization-evaluator) value)
  (let* ((result 0)
         (min (measure-start e))
         (max (measure-end e))
         (length (length value))
         (delta (coerce (/ (- max min) (expt 2 length)) 'double-float)))
    (declare (integer result))
    (dotimes (i length)
      (incf result (* (expt 2 i) (aref value i))))
    (+ min (* delta result))))
