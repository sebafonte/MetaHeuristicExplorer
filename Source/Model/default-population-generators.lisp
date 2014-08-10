
(defun initialize-default-population-generators ()
  (system-add
   (make-instance 'random-binary-generator 
                  :name 'random-binary-initializer
                  :description "Random binary")
   (make-instance 'random-trees-generator 
                  :name 'random-trees-initializer
                  :description "Random trees")
   (make-instance 'random-trees-generator-with-operator
                  :name 'random-trees-cfg-initializer
                  :description "Random trees using"
                  :operator (system-get-copy 'initial-random-create-cfg-1))
   (make-instance 'fixed-solutions-generator 
                  :name 'fixed-expressions-initializer
                  :description "Fixed expressions")
   (make-instance 'ramped-half-and-half-generator 
                  :description "Ramped half & half"
                  :name 'ramped-half-and-half-initializer)
   (make-instance 'sample-linear-ordering-population-generator 
                  :name 'sample-lop-initializer
                  :description "Sample LOP")
   (make-instance 'sample-vrp-population-generator 
                  :name 'sample-vrp-initializer
                  :description "Sample VRP")
   (make-instance 'property-sampling-population-generator 
                  :name 'sample-property-sampling-initializer
                  :description "Sample property sampling")
   (make-instance 'polynomial-sample-generator 
                  :name 'sample-random-polynomial-generator
                  :description "Sample polynomial")
   (make-instance 'search-task-sample-generator 
                  :name 'sample-search-task-generator
                  :description "Sample search task")
   (make-instance 'evolutive-algorithm-sample-generator 
                  :name 'evolutive-algorithm-sample-generator
                  :description "Sample algorithm")
   (make-instance 'task-best-objects-initializer 
                  :name 'task-best-objects-initializer
                  :description "Task best objects")
   (make-instance 'task-random-objects-initializer 
                  :name 'task-random-objects-initializer
                  :description "Task random objects")))


(defmethod possible-initialization-methods ((o t))
  (append
   (possible-initialization-methods-for o)
   (common-initialization-methods)))

(defmethod common-initialization-methods ()
  (list 
   (system-get 'fixed-expressions-initializer)
   (system-get 'task-best-objects-initializer)))

(defmethod possible-initialization-methods-for ((o t))
  nil)

(defmethod possible-initialization-methods-for ((o entity-function-maximization))
  (list 
   (system-get 'random-binary-initializer)))

(defmethod possible-initialization-methods-for ((o entity-sample-vrp))
  (list 
   (system-get 'sample-vrp-initializer)))

(defmethod possible-initialization-methods-for ((o entity-linear-ordering))
  (list 
   (system-get 'sample-lop-initializer)))

(defmethod possible-initialization-methods-for ((o entity-function))
  (list 
   ;; Common, #TODO: 
   (system-get 'random-trees-initializer)
   (system-get 'random-trees-cfg-initializer)
   (system-get 'ramped-half-and-half-initializer)
   (system-get 'sample-random-polynomial-generator)))

(defmethod possible-initialization-methods-for ((o configurable-search-algorithm))
  (list 
   (system-get 'random-trees-cfg-initializer)))

(defmethod possible-initialization-methods-for ((o entity-pattern))
  (list 
   (system-get 'random-trees-cfg-initializer)))

