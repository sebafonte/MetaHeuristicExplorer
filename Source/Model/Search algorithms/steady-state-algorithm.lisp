(defclass steady-state-algorithm (evolutionary-algorithm)
  ((inverse-normalized-fitness :initform nil :accessor inverse-normalized-fitness)
   (replacement-strategy :initarg :replacement-strategy :accessor replacement-strategy)
   (hash-table-clear-interval :initarg :hash-table-clear-interval :accessor hash-table-clear-interval)
   (simplification-optimization :initarg :simplification-optimization :accessor simplification-optimization)))


(defmethod initialize-properties :after ((a steady-state-algorithm))
  "Initialize <a> properties."
  ;; Constant optimization properties
  (let ((local-optimization (copy (system-get 'default-iterational-optimization-strategy)))
        (simplification-optimization (copy (system-get 'default-iterational-optimization-strategy))))
    (setf (subject local-optimization) a
          (subject simplification-optimization) a
          (max-iterations local-optimization) nil
          (max-iterations simplification-optimization) nil)
    ;; Properties
    (add-properties-from-values
     a
     (:name 'replacement-strategy :label "Replacement strategy" :accessor-type 'accessor-accessor-type 
      :possible-values (system-selection-methods) :editor 'configurable-copy-list-editor
      :data-type 'object :category "Methods" :default-value (system-get 'inverse-fitness-selection-method))
     (:name 'elite-manager :label "Elite manager" :accessor-type 'accessor-accessor-type :data-type 'object
      :default-value (make-instance 'elite-manager) :editor 'button-editor :category "Methods" :visible nil)
     (:name 'hash-table-clear-interval :label "Uniques registry clear interval" :accessor-type 'accessor-accessor-type 
      :data-type 'object :editor 'lisp-editor :default-value 300)
     (:name 'breed-method :label "Breed method" :accessor-type 'accessor-accessor-type :data-type 'object 
      :default-value (system-get 'full-population-breeding) :possible-values (list (system-get 'full-population-breeding))
      :editor 'configurable-copy-list-editor :category "Methods")
     (:name 'local-optimization :label "Local optimization" :accessor-type 'accessor-accessor-type 
      :data-type 'symbol :default-value local-optimization :possible-values (optimization-strategies) 
      :editor 'configurable-copy-list-editor :category "Methods")
     (:name 'simplification-optimization :label "Simplification optimization" :accessor-type 'accessor-accessor-type 
      :data-type 'symbol :default-value simplification-optimization :possible-values (optimization-strategies) 
      :editor 'configurable-copy-list-editor :category "Methods"))))

(defmethod search-loop ((a steady-state-algorithm) seed) 
  "Search method implementation for <a>." 
  (setf *seed* (coerce seed 'double-float))
  (let ((elite-manager (elite-manager a)))
    (generate-initial-population a)
    (do ((iteration 1 (1+ iteration)))
        ((test-termination a iteration))
      (setf (iteration a) iteration)
      (check-clear-hash-table a)
      (breed a)
      (execute-constant-local-optimization a)
      (execute-simplification-local-optimization a)
      (normalize-and-sort-population-fitness (population a))
      (trigger a :progress-change iteration))))

(defmethod check-clear-hash-table ((a steady-state-algorithm))
  "Check to clear repeated individual control hash table of <a>."
  (when (= 0 (mod (iteration a) (hash-table-clear-interval a)))
    (clrhash (registry a))))

(defmethod replace-child ((a steady-state-algorithm) child)
  "Replace <child> in <a> population."
  (let* ((p (population a))
         (replacement (first (perform-selection (replacement-strategy a) p 1)))
         (index-replacement (index-individual p replacement)))
    (setf (aref (individuals-array p) index-replacement) child)))

(defmethod test-termination ((a steady-state-algorithm) iteration)
  "Answer whether the steady state search has to finish."
  (or (> iteration (max-iterations a))
      (>= (fitness (best-individual a)) 
          (solution-fitness (fitness-evaluator a)))))

(defmethod population-replacement-strategies ((a steady-state-algorithm))
  "Answer possible individual replacement method for <a>."
  (list 
   (system-get 'default-worst-replacement-strategy)))

(defmethod default-progress-indicator-value ((a steady-state-algorithm))
  (iteration a))

(defmethod execute-simplification-local-optimization ((a steady-state-algorithm))
  (check-execute-optimization (simplification-optimization a)))

;; #TODO: Add a higher level progress indicator
(defmethod max-generations ((a steady-state-algorithm))
  (max-iterations a))

(defmethod generation ((a steady-state-algorithm))
  (iteration a))
