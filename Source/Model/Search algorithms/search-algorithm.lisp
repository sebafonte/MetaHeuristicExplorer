
(defclass search-algorithm (object-with-properties)
  ((name :initarg :name :initform "Search algorithm" :accessor name)
   (description :initarg :description :accessor description)
   (max-iterations :initarg :max-iterations :initform 1000 :accessor max-iterations)
   (max-evaluations :initarg :max-evaluations :accessor max-evaluations)
   (iteration :initform 0 :accessor iteration)
   (context :initarg :context :initform nil :accessor context)))


(defmethod print-object ((o search-algorithm) seq)
  (format seq "~A" (description o)))

(defmethod initialize-properties :after ((a search-algorithm))
  "Initialize <a> properties."
  (add-properties-from-values
   a 
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type :data-type 'symbol
    :default-value 'search-algorithm :editor 'text-editor)  
   (:name 'description :label "Description" :accessor-type 'accessor-accessor-type 
    :data-type 'string :editor 'text-editor :default-value "Description")
   (:name 'iteration :label "Iteration" :default-value 0 :accessor-type 'accessor-accessor-type 
    :data-type 'integer :read-only t :editor 'number-editor)
   (:name 'max-iterations :label "Max iterations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 10000 :editor 'number-editor)
   (:name 'max-evaluations :label "Max evaluations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 10000000 :min-value 0 :max-value 10000000 :default-value nil :editor 'number-editor)
   (:name 'context :label "Context" :accessor-type 'accessor-accessor-type :data-type 'object 
    :editor 'button-editor)))

(defmethod fitness-function ((a search-algorithm))
  "Answer <a> fitness funtion."
  (fitness-function (context a)))

(defmethod target-program ((a search-algorithm))
  "Answer <a> objective sample."
  (target-program (context a)))

(defmethod objective-class ((a search-algorithm))
  "Answer <a> objective class."
  (objective-class (context a)))

(defmethod solution-fitness ((a search-algorithm))
  "Answer <a> minimum fitness value for a good solution."
  (solution-fitness (context a)))

(defmethod drawablep ((a search-algorithm))
  "Answer whether <o> can be displayed on the GUI."
  nil)

(defmethod evolvablep ((o search-algorithm))
  "Answer whether <o> can be evolved."
  nil)

(defmethod search-loop :before ((a search-algorithm) seed)
  "Takes care of finish actions when performing a search-loop on <a>."
  (setf (state (context a)) 'RUNNING)
  (trigger a :algorithm-start))

(defmethod search-loop :after ((a search-algorithm) seed)
  "Takes care of finish actions when performing a search-loop on <a>."
  (trigger a :algorithm-end)
  (setf (state (context a)) 'FINISHED))

(defmethod default-progress-indicator-value ((a search-algorithm))
  (iteration a))

(defmethod fitness-evaluator ((a search-algorithm))
  (fitness-evaluator (context a)))

(defmethod initialize-objective-data ((a search-algorithm))
  (initialize-fitness-data (fitness-evaluator a)))

(defmethod initialize-objective-data :before ((a search-algorithm))
  (trigger a :initialize-fitness-data-start))

(defmethod initialize-objective-data :after ((a search-algorithm))
  (trigger a :initialize-fitness-data-end))

(defmethod ensure-objective-data-initialized ((a search-algorithm))
  (ensure-fitness-data-initialized (fitness-evaluator a)))

;; Language object hook utility functions
(defmethod language ((a search-algorithm))
  (language (context a)))

(defmethod grammar ((a search-algorithm))
  (grammar (language a)))

(defmethod max-size ((a search-algorithm))
  (max-size-for (language a) (fitness-evaluator a)))

(defmethod max-depth ((a search-algorithm))
  (max-depth (language a)))

(defmethod functions ((a search-algorithm))
  (functions (language a)))

(defmethod variables ((a search-algorithm))
  (variables (language a)))

(defmethod terminals ((a search-algorithm))
  (terminals (language a)))

(defmethod simplification-function ((a search-algorithm))
  (simplification-function (language a)))

(defmethod simplification-patterns ((a search-algorithm))
  (simplification-patterns (language a)))

(defmethod max-size-new-individuals ((a search-algorithm))
  (max-size-new-individuals (language a)))

(defmethod max-depth-new-individuals ((a search-algorithm))
  (max-depth-new-individuals (language a)))

(defmethod operators ((a search-algorithm))
  (operators (language a)))

(defmethod evaluate ((a search-algorithm) (o entity))
  "Evaluate <o> using <a> fitness evaluator."
  (evaluate (fitness-evaluator (context a)) o))

(defmethod evaluate ((a search-algorithm) (p population)) 
  "Evaluate <p> individuals with <a>."
  (evaluate (fitness-evaluator a) p))

(defmethod evaluate :after ((a search-algorithm) (o entity))
  "Evaluate <o> using <a> fitness evaluator."
  (trigger a :individual-evaluated o))

(defmethod evaluate :after ((a search-algorithm) (p population))
  "Evaluate <p> individuals with <a>."
  (trigger a :population-evaluated p))

;; #TODO: Make it, move from here
(defmethod evaluate-single-color-penalty ((a search-algorithm) (o entity-image-bw))
  "Answer the fitness of image <o>. 
   #NOTE: Penalty method is assigned for flat color images."
  (setf (fitness o) 5))

(defmethod register-individual ((a search-algorithm) o)
  "Register <o> on <a>."
  (setf (gethash (program o) (registry a)) t))

(defmethod abstractp ((o (eql 'search-algorithm)))
  "Answer whether <o> is abstract."
  t)

(defmethod make-objective ((o search-algorithm) &optional exp)
  (make-instance (objective-class o) exp))

(defmethod make-objective ((o search-task) &optional exp)
  (make-instance (objective-class o) exp))

(defmethod make-objective ((o symbol) &optional exp)
  (make-instance o exp))