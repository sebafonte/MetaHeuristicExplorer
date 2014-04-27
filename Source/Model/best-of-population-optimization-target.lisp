
(defclass best-of-population-optimization-target (optimization-target)
  ((size :initarg :size :accessor size)))


(defmethod initialize-properties :after ((o best-of-population-optimization-target))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'size :label "Size" :accessor-type 'accessor-accessor-type :default-value 1 
    :data-type 'size :editor 'number-editor)))

(defmethod target ((o best-of-population-optimization-target) (strategy optimization-strategy))
  (mapcar (lambda (object) 
            (make-instance 'object-in-search
                           :context (context (subject strategy))
                           :object object))
          (best-individuals (population (subject strategy)) (size o))))


