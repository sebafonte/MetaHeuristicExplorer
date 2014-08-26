
(defclass sample-vrp-population-generator (population-generator)
  ((generation-method :initarg :generation-method :accessor generation-method)))


(defmethod initialize-properties :after ((object sample-vrp-population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'generation-method :label "Generation method" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value (first (possible-generation-methods object)) :editor 'list-editor
    :possible-values (possible-generation-methods object))))

(defmethod possible-generation-methods ((object sample-vrp-population-generator))
  '(clark-and-wright-generation
    random-tour-variable-vehicles
    random-tour-fixed-vehicles
    radial-scan-generation-method
    one-vehicle-per-city-method))

(defmethod generate-population ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (funcall (generation-method object) object algorithm))

(defmethod clark-and-wright-generation ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generation method which fills the entire population of the cw solution or it mutations."
  (let* ((cw-solution (clark-and-wright-parallel (fitness-evaluator algorithm)))
         (population-size (population-size algorithm))
         (individuals (make-array population-size)))
    (dotimes (i (population-size algorithm))
      (let ((new-object (make-instance 'entity-sample-vrp :expresion cw-solution)))
        (evaluate algorithm new-object)
        (setf (aref individuals i) new-object)))
    (make-instance 'population :individuals-array individuals)))

(defmethod one-vehicle-per-city-method ((object sample-vrp-population-generator) (algorithm search-algorithm))
 "Generation method which implements radial scanning heuristic."
 (let* ((population-size (population-size algorithm))
        (population (make-array population-size)))
   (dotimes (i population-size)
     (let* ((plan (list (list 0 1)))
            (individual (make-objective algorithm plan)))
       (evaluate algorithm individual)
       (setf (aref population i) individual)))
   (make-instance 'population :individuals-array population)))

(defmethod random-tour-variable-vehicles ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generation method which generates random tours for a variable amount of vehicles."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size))
         (min-tour-size 1)
         (max-tour-size 10)
         (delta (- max-tour-size min-tour-size))
         (cities-count (1- (cities-count (fitness-evaluator algorithm)))))
    (dotimes (i (population-size algorithm))
      (let ((tour-size (+ min-tour-size (mod i delta)))
            (city-list (loop for i from 1 to cities-count collect i))
            (hash-table (make-hash-table)))
        (dotimes (j cities-count)
          (let ((tour-index (1+ (mod j tour-size)))
                (next-city (select-random-city object city-list)))
            (removef city-list next-city)
            (appendf (gethash tour-index hash-table) (list next-city))))
        (let ((individual (construct-individual-from object hash-table)))
          (evaluate algorithm individual)
          (setf (aref population i) individual))))
    (make-instance 'population :individuals-array population)))

(defmethod select-random-city ((object sample-vrp-population-generator) list)
  (nth (random-integer 0 (length list)) list))

(defmethod construct-individual-from ((object sample-vrp-population-generator) hash-table)
  (let ((tour-list))
    (dolist (key (keys hash-table))
      (appendf tour-list (list (gethash key hash-table))))
    (make-instance 'entity-sample-vrp :expresion tour-list)))

(defmethod radial-scan-generation-method ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generation method which implements radial scanning heuristic."
  nil)

(defmethod random-tour-fixed-vehicles ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generation method which generates random tours for a fixed amount of vehicles."
  nil)