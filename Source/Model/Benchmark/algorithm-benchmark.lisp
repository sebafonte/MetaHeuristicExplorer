
(defclass algorithm-benchmark (base-benchmark)
  ())


(defmethod initialize-properties :after ((o algorithm-benchmark))
  "Initialize <object> properties."
  (add-properties-from-values
   o    
   ;; Fitness
   (:name 'best-individual :label "Best individual" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'best-fitness :label "Best fitness" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'mean-fitness :label "Mean fitness" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'fitness-std :label "Std fitness" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   ;; Size
   (:name 'best-size :label "Best size" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'mean-size :label "Mean size" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0) 
   (:name 'min-size :label "Min size" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'size-std :label "Std size" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   ;; Diversity
   (:name 'diversity-size :label "Diverity size" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'diversity-fitness :label "Diverity fitness" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   ;; Time
   (:name 'evaluations :label "Evaluations" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)
   (:name 'time :label "Time" :accessor-type 'property-accessor-type
    :data-type 'number :default-value 0)))

(defmethod initialize-log-inspectors :after ((o algorithm-benchmark) subject)
  nil)

#|
;; #TODO: Move to this scheme this piece of code:
(defmethod diversity-vs-x ((p population) x)
  "Answer <p> diversity level vs <x> time variable."
  (let ((table (make-hash-table)))
    (dotimes (i (size p))
      (setf (gethash (if (consp p)
                         (car (program (get-individual p i)))
                       p)
                     table) 1)
    (hash-table-count table))))
|#
