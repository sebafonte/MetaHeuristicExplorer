
(defclass gp-crossover-tune-benchmark (algorithm-benchmark)
  ())


;; #TODO: Should adjust properties depending on the problem object
(defmethod initialize-properties :after ((o gp-crossover-tune-benchmark))
  "Initialize <object> properties."
  (add-properties-from-values
   o
   (:name 'crossover-usage :label "Crossover usage" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'mutate-usage :label "Mutate usage" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'branch-delete-usage :label "Branch delete usage" :accessor-type 'property-accessor-type
    :data-type 'branch-delete-usage :default-value 0)
   (:name 'crossover-sucess :label "Crossover sucess" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'mutate-sucess :label "Mutate sucess" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'branch-delete-sucess :label "Branch delete sucess" :accessor-type 'property-accessor-type
    :data-type 'branch-delete-usage :default-value 0)))

(defmethod initialize-log-inspectors :after ((o gp-crossover-tune-benchmark) subject)
  nil)
