
(defclass randomized-nearest-neighbor-generator (object-with-properties)
  ((name :initarg :name :accessor name)
   (strategy :initarg :strategy :accessor strategy)))


(defmethod initialize-properties :after ((o randomized-nearest-neighbor-generator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :default-value "Randomized nearest neighbor heuristic" :data-type 'string :editor 'text-editor)
   (:name 'strategy :label "Selection strategy" :accessor-type 'accessor-accessor-type :default-value 'first 
    :data-type 'object :editor 'text-editor :visible nil)))

(defmethod generate ((o randomized-nearest-neighbor-generator) evaluator)
  "Answer a new individual constructed with RNNH."
  (let ((route (list (first (cities-description evaluator)))))
    (while-do
     (let* ((order-function (order-function o evaluator route))
            (possible (set-difference (cities-description evaluator) route))
            (ordered-possible (sort possible order-function))
            (item (select-city-target o ordered-possible)))
       (appendf route item)
       (removef ordered-possible item)
       ordered-possible))
    route))
  
(defmethod order-function ((o randomized-nearest-neighbor-generator) evaluator route)
  "Answer a function to order possible neighbors of <route>."
  (lambda (a b) (< (distance-between evaluator (first route) a)
                   (distance-between evaluator (first route) b))))

(defmethod select-city-target ((o randomized-nearest-neighbor-generator) list)
  "Answer the selected city from <list>."
  (apply (strategy o) (list list)))


#|
(let ((evaluator (make-instance 'entity-dvrp-evaluator :description "Test" :max-distance 10000))
      (generator (make-instance 'randomized-nearest-neighbor-generator))
      (object (make-instance 'entity-sample-vrp)))
  (setf (max-capacity evaluator) 8)
  (setf (cities-description evaluator) '(0 1 2 3 4 5 6))
  (setf (demand-description evaluator) '(0 3 2 4 3 2 2))
  (setf (demand-matrix evaluator) (to-array (demand-description evaluator)))
  (setf (costs-matrix evaluator)
        #2a((0  64   58  54  41  58 41)
            (64 0    70  95 103  81 40)
            (58 70    0  36  92 113 81)
            (54 95   36   0  72 112 91)
            (41 103  92  72   0  61 71)
            (58 81  113 112  61   0 41)
            (41 40   81  91  71  41  0)))
  (setf (program object) (generate generator evaluator)))
|#

