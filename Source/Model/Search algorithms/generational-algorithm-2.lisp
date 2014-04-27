(defclass generational-algorithm-2 (evolutionary-algorithm)
  ((max-generations :initarg :max-generations :initform 100 :accessor max-generations)
   (generation :initform 0 :accessor generation)))


(defmethod search-loop ((a generational-algorithm-2) seed)
  "Search method implementation for <a>." 
  (setf *seed* (coerce seed 'double-float))
  (let ((new-population (make-array (population-size a)))
        (registry (registry a))
        (elite-manager (elite-manager a)))
    ;; Initialize population
    (generate-initial-population a)
    ;; Generational loop
    (do ((generation 1 (1+ generation)))      
        ((test-termination a generation))
      (setf (generation a) generation)
      (clrhash registry)
      (register-elites a)
      ;; Breed population
      (let ((population (population a)))
        ;; Fill population with distinct individuals (using #'create-distinct-individual)
        (dotimes (i (population-size a))
          (let ((hijo (create-distinct-individual a)))
            (setf (aref new-population i) hijo)))
        ;; Reinsert population elites and normalize/order by fitness
        (setf (individuals-array population) new-population)
        (check-and-update-population elite-manager population)
        (execute-constant-local-optimization a)
        (normalize-and-sort-population-fitness population)
        ;; Register context properties as observations
        (save-observations (context a))))))

(defmethod test-termination ((a generational-algorithm-2) generation)
  "Answer whether <a> has finished at <generation>."
  (let ((best-individual (best-individual a))
        (solution-fitness (solution-fitness (fitness-evaluator a))))
    (or (> generation (max-generations a))
        (better-than-fitness-value best-individual solution-fitness)
        (= (fitness best-individual) solution-fitness))))

(defmethod default-progress-indicator-value ((a generational-algorithm-2))
  (generation a))

(defmethod all-individuals ((a generational-algorithm-2))
  (append (individuals (population a))
          (elites (elite-manager a))))
