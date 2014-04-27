
(defclass generational-algorithm (evolutionary-algorithm)
  ((max-generations :initarg :max-generations :initform 100 :accessor max-generations)
   (generation :initform 0 :accessor generation)))


(defmethod initialize-properties :after ((a generational-algorithm))
  "Initialize <a> properties."
  ;; Initialize optimization strategy
  (let ((constant-optimization (system-get-copy 'default-generational-optimization-strategy)))
    (setf (subject constant-optimization) a
          (max-generations constant-optimization) nil)
    ;; Initialize default properties
    (add-properties-from-values
     a
     (:name 'max-generations :label "Max generations" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :min-value 0 :max-value 100000 :default-value 200 :editor 'number-editor
      :object-parameter t)
     (:name 'generation :label "Generation" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :default-value 0 :read-only t :editor 'number-editor)
     (:name 'breed-method :label "Breed method" :accessor-type 'accessor-accessor-type :data-type 'object 
      :default-value (system-get 'full-population-breeding) :possible-values (list (system-get 'full-population-breeding))
      :editor 'configurable-copy-list-editor :category "Methods")
     (:name 'constant-optimization :label "Constant optimization" :accessor-type 'accessor-accessor-type 
      :data-type 'object :default-value constant-optimization :possible-values (optimization-strategies) 
      :editor 'configurable-copy-list-editor))))

(defmethod search-loop ((a generational-algorithm) seed)
  "Search method implementation for <a>." 
  (setf *seed* (coerce seed 'double-float))
  ;; Initialize population
  (generate-initial-population a)
  ;; Generational loop
  (do ((generation 1 (1+ generation)))      
      ((test-termination a generation))
    (setf (generation a) generation)
    ;; Breed population
    ;; #TODO: Move to breeder
    (clear-registry a)
    (register-elites a)
    (breed a)
    (check-and-update-population (elite-manager a) (population a))
    (execute-constant-local-optimization a)
    (normalize-and-sort-population-fitness (population a))
    (trigger a :progress-change a)))

(defmethod clear-registry ((a evolutionary-algorithm))
  (clrhash (registry a)))

(defmethod clear-registry :after ((a evolutionary-algorithm))
  (trigger a :algorithm-repeat-registry-cleaned))

(defmethod check-for-possible-best-individual ((a search-algorithm) (object entity))
  "Verifies if <a> has to register <object> as the best individual found."
  (let ((best (best-individual a)))
    (if (or (better-than object best)
            (and (= (fitness best)
                    (fitness object))
                 (< (tree-size (program object))
                    (tree-size (program best)))))
        (setf (best-individual a) object))))

(defmethod check-for-possible-best-individual ((a search-algorithm) (object entity-linear-ordering))
  "Verifies if <a> has to register <object> as the best individual found."
  (if (better-than object (best-individual a))
      (setf (best-individual a) object)))

(defmethod test-termination ((a generational-algorithm) generation)
  "Answer whether <a> has finished at <generation>."
  (let ((best-individual (best-individual a))
        (solution-fitness (solution-fitness (fitness-evaluator a))))
    (or (> generation (max-generations a))
        (better-than-fitness-value best-individual solution-fitness)
        (= (fitness best-individual) solution-fitness))))

(defmethod default-progress-indicator-value ((a generational-algorithm))
  (generation a))

(defmethod all-individuals ((a generational-algorithm))
  (append (individuals (population a))
          (elites (elite-manager a))))
