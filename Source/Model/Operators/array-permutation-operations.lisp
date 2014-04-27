(defclass array-permutation (unary-genetic-operator)
  ((mutation-points-number :initarg :mutation-points-number :accessor mutation-points-number)
   (consecutive :initarg :consecutive :accessor consecutive)))


;; #TODO: Refactor to make entity-linear-ordering use this
#|
(defmethod initialize-properties :after ((object array-permutation))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'permutations-number :label "Points" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 1 :max-value 10000 :default-value 1 :editor 'function-editor)))

(defclass array-row-permutation (array-permutation)
  ())

(defmethod operate ((o array-row-permutation) algorithm genes)
  (let ((gen (copy (first genes))))
    (dotimes (i (permutations-number o))
      (let ((source ())
            (target ()))
        (dotimes (y size)
          (setf aux (aref target y gen)
                (aref target y gen) (aref source y gen)
                (aref source y gen) aux))))
    gen))


(defclass array-column-permutation (array-permutation)
  ())
    
(defmethod operate ((o array-column-permutation) algorithm genes)
  (let ((gen (copy (first genes))))
    (dotimes (i (permutations-number o))
      (let ((source ())
            (target ()))
        (dotimes (y size)
          (setf aux (aref target y gen)
                (aref target y gen) (aref source y gen)
                (aref source y gen) aux))))
    gen))
|#