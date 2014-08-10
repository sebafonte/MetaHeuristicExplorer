
(defclass combined-population-generator (population-generator)
  ((operations :initarg :operations :accessor operations)))


(defmethod initialize-properties :after ((object combined-population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'combined-population-generator :editor 'symbol-editor)
   (:name 'operations :label "Operations" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :editor 'list-editor)))

(defmethod generate-population ((p combined-population-generator) (a search-algorithm))
  "Generate <p> for <a>."
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (max-attempts (max-unique-iterations a))
         (attempts 0))
    (do ((i 0))
        ((>= i population-size))
      (let ((child (create-with-random-operation p a)))
        (cond ;; 1- New and not in registry
              (t 
               (evaluate a child)
               (setf (gethash (program child) (registry a)) t
                     attempts 0
                     (aref population i) child) 
               (incf i))
              ;; 2 - New and found intro registry and max attempts have reached
              ((> attempts max-attempts)
               (incf min-tree-depth))
              ;; 3 - Max attempts have'nt been reached
              (t (incf attempts)))))
    (clrhash (registry a))
    (make-instance 'population :individuals-array population)))

(defmethod create-with-random-operation ((p combined-population-generator) (a search-algorithm))
  "Answer a new created object using <p> creation operators."
  (operate 
   (operation (first (random-elements (operations p) 1)))
   a
   nil))