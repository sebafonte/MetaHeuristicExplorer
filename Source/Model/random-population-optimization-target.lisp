
(defclass random-population-optimization-target (optimization-target)
  ((size :initarg :size :accessor size)))


(defmethod initialize-properties :after ((o random-population-optimization-target))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'size :label "Size" :accessor-type 'accessor-accessor-type :default-value 1 
    :data-type 'size :editor 'number-editor
    ;; #TODO: Max value should be population size value
    :object-parameter t :min-value 1 :max-value 1000)))

(defmethod target ((o random-population-optimization-target) (strategy optimization-strategy))
  (mapcar (lambda (object) 
            (make-instance 'object-in-search
                           :context (context (subject strategy))
                           :object object))
          (random-elements 
           (individuals (population (subject strategy)))
           (size o))))
