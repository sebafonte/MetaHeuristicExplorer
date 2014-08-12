(defvar *seed* :unbound)


(defclass evolutionary-algorithm (search-algorithm)
  ((population :initarg :initial-population :accessor population)
   (population-size :initarg :population-size :accessor population-size)
   (selection-method :initarg :selection-method :accessor selection-method)
   (initialization-method :initarg :initialization-method :accessor initialization-method)
   (breed-method :initarg :breed-method :accessor breed-method)
   (registry :initform (make-hash-table :test #'equal) :accessor registry)
   (max-unique-iterations :initarg :max-unique-iterations :accessor max-unique-iterations)
   (local-optimization :initarg :local-optimization :accessor local-optimization)
   (elite-manager :initarg :elite-manager :accessor elite-manager)))


(defmethod initialize-properties :after ((a evolutionary-algorithm))
  "Initialize <a> properties."
  (add-properties-from-values
   a
   (:name 'population :label "Population" :accessor-type 'accessor-accessor-type 
    :data-type 'population :default-value nil :editor 'button-editor)
   (:name 'population-size :label "Population size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 1000000 :default-value 100 :editor 'lisp-editor
    :category "Parameters")
   (:name 'selection-method :label "Selection method" :accessor-type 'accessor-accessor-type
    :data-type 'symbol :default-value (system-get 'tournament-selection-method)
    :possible-values (system-selection-methods) :editor 'configurable-copy-list-editor
    :category "Methods")
   (:name 'initialization-method :label "Initialization method" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value (system-get 'random-trees-initializer) 
    :editor 'configurable-copy-list-editor :category "Methods"
    :dependency (make-possible-class-dependency 'objective-class)
    :default-value-function (lambda (objective-class) (copy-cyclic (first (possible-initialization-methods (make-instance objective-class)))))
    :possible-values-function (lambda (objective-class) (possible-initialization-methods (make-instance objective-class))))
   (:name 'breed-method :label "Breed method" :accessor-type 'accessor-accessor-type :data-type 'object 
    :editor 'configurable-copy-list-editor :category "Methods")
   (:name 'elite-manager :label "Elite manager" :accessor-type 'accessor-accessor-type 
    :data-type 'object :default-value (make-instance 'elite-manager) :editor 'button-editor
    :category "Methods")
   (:name 'max-unique-iterations :label "Max iterations unique" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 10 :editor 'one-line-lisp-editor
    :category "Parameters")
   (:name 'local-optimization :label "Local optimization" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value nil :possible-values (optimization-strategies) 
    :editor 'configurable-copy-list-editor :category "Methods")))

(defmethod copy ((object evolutionary-algorithm))
  (setf (subject (local-optimization object)) nil)
  (let ((new-instance (copy-instance object)))
    (setf (subject (local-optimization new-instance)) new-instance
          (subject (local-optimization object)) object)
    new-instance))

(defmethod all-individuals ((a evolutionary-algorithm))
  (individuals (population a)))

;; #NOTE: - Elite manager has the best in this kind of algorithm
;;        - Changes if new objective is measured for on elites
;;        - Has a ugly dependence on algorithm code
(defmethod best-individual ((a evolutionary-algorithm))
  "Anwer the best individual found by <a>."
  (let ((manager (elite-manager a)))
    (if (and (population a) (individuals (population a)))
        (if (elites manager)
            (best (elite-manager a))
          (first (sort (individuals (population a)) 'lambda-default-fitness-comparer))))))

(defmethod select-genetic-operation ((a evolutionary-algorithm))
  "Answer a genetic operation for <a>."
  (get-random-element (operators a)))

(defmethod generate-initial-population ((a evolutionary-algorithm))
  "Generate <a> initial population."
  (setf (population a) (generate-population (initialization-method a) a))
  (check-elite (elite-manager a) (population a))
  (normalize-population-fitness (population a) #'fitness))

(defmethod generate-initial-population :before ((a evolutionary-algorithm))
  (trigger a :initial-population-generation-start))

(defmethod generate-initial-population :after ((a evolutionary-algorithm))
  (trigger a :initial-population-generation-end (population a)))

(defmethod execute-constant-local-optimization ((a evolutionary-algorithm))
  (check-execute-optimization (local-optimization a)))

(defmethod breed ((a evolutionary-algorithm))
  (breed-population (breed-method a) a))

(defmethod repeat-control ((a evolutionary-algorithm))
  (and (numberp (max-unique-iterations a))
       (not (= (max-unique-iterations a) 0))))

(defmethod abstractp ((o (eql 'evolutionary-algorithm)))
  "Answer whether <o> is abstract."
  t)

(defmethod register-elites ((a evolutionary-algorithm))
  "Register algorithm elites (from population to manager and into registry)."
  (let ((manager (elite-manager a)))
    (check-elite manager (population a))
    (update-population manager (population a))
    (dolist (i (elites manager))
      (register-individual a i))))