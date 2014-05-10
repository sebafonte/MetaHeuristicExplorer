
(defclass entity-function-maximization-evaluator (entity-evaluator)
  ((target-program :initarg :target-program :accessor target-program)
   (compiled-target-program :initarg :compiled-target-program :accessor compiled-target-program)
   (measure-start :initarg :measure-start :accessor measure-start)
   (measure-end :initarg :measure-end :accessor measure-end)))


(defmethod initialize-properties :after ((o entity-function-maximization-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'target-program :label "Target program" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'lisp-editor
    :default-value '(* x x)
    :possible-values '((* x x)
                       (- (sin x) (cos x))
                       (exp (- 1 x) (* x 0.25))
                       (+ x 7 (/ x 17))))
   (:name 'measure-start :label "Measure start" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 0 :editor 'number-editor :subject o)
   (:name 'measure-end :label "Measure end" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 10 :editor 'number-editor :subject o)))

(defmethod (setf target-program) (value (e entity-function-maximization-evaluator))
  "Initialize <o> fitness data."
  (setf (slot-value 'target-program o) value
        (compiled-target-program o) (compile nil `(lambda (x) ,value))))

(defmethod evaluate-distance ((e entity-function-maximization-evaluator) (o entity-function-maximization))
  "Evaluate using absolute difference with target values."
  (setf (fitness o) (funcall (compiled-target-program o) (program o))))

(defmethod objetive-class ((evaluator entity-function-maximization-evaluator))
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