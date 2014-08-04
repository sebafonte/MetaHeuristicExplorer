
(defclass search-task-grammar (context-free-grammar)
  ())


(defparameter *search-task-grammar-tokens*
  '(;; Object
    ;(MAKE-OBJ :search-object)
    ;; Connectors
    (BEST-OF-TASK :best-of-task)
    (BEST-OF-TASKS :best-of-tasks)
    ;; Task specifier
    (MAKE-TASK :task-description)
    ;; Task builder
    (MAKE-BUILDER-IT :iterative-builder)
    ;; Algorithms
    (MAKE-ALG-GG :algorithm-generational)
    (MAKE-ALG-SS :algorithm-steady-state)
    ;; Generators
    (MAKE-GN-RND :generator-random-object)
    (MAKE-GN-RND-ST :generator-random-search-task)
    (MAKE-GN-BESTS-ST :generator-bests-search-task)
    ;; Languages
    (MAKE-LG :language-object)
    (MAKE-LG-ST :language-search-task)
    ;; Language functions
    (MAKE-LGF :language-functions-object)
    (MAKE-LGF-ST :language-functions-search-task)
    ;; Fitness evaluators
    (MAKE-FE :fitness-evaluator-object)
    ;; Operators usage
    (MAKE-OP :operator-description-object)
    ;; Selection methods
    (MAKE-SM-TOURNAMENT :tournament-selection-method)
    (MAKE-SM-RANK :ranking-selection-method)
    (MAKE-SM-INDEX :index-selection-method)
    (MAKE-SM-RANDOM :random-selection-method)
    (MAKE-SM-BEST :best-selection-method)
    (MAKE-SM-WORST :worst-selection-method)
    (MAKE-SM-IRANKING :inverse-ranking-selection-method)
    (MAKE-SM-IINDEX :inverse-index-selection-method)
    ;; Elite manager
    (MAKE-EM :elite-manager)))


(defun search-task-grammar-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol (search-task-grammar-get-token grammar symbol)
      nil)))

(defun search-task-grammar-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (if (equal token-type :unknown) 
        (setf token-type 
              (if (numberp word) :constant 
                (if (listp word) :list))))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-search-task-grammar-parser (name)
  (search-task-grammar-parser name))

(defun search-task-grammar-parser (name)
  (eval
   `(defparser ,name
               ;; Start
               ((start expresion) $1)
               ;; Object connectors
               ((expresion object)
                $1)
               ((object best-of-task-description)
                `(:search-object-description ,$1))
               ((object best-of-tasks-description)
                `(:search-object-description ,$1))
               ;; Search tasks root node
               ((best-of-task-description :open :best-of-task task-description :close)
                `((:best-of-task BEST-OF-TASK) ,$3))
               ((best-of-tasks-description :open :best-of-tasks task-description-list :close)
                `((:best-of-tasks BEST-OF-TASKS) ,$3))
               ((task-description-list task-description-list task-description)
                `(:task-description-list ,$1 ,$2))
               ;; Object descriptor
               ((task-description-list task-description)
                `(:task-description-list ,$1))
               ((task-description :open :task-description 
                                  builder-description algorithm-description language-description 
                                  generator-description fitness-evaluator-description
                                  :close)
                `((:task-description MAKE-TASK) ,$3 ,$4 ,$5 ,$6 ,$7))
               ;; Task builder
               ((builder-description :open :iterative-builder iterations-description :close)
                `((:builder-description MAKE-BUILDER-IT) ,$3))
               ((iterations-description :constant)
                `,$1)
               ;; Search algorithms (2 types)
               ((algorithm-description generational-algorithm-description)
                `(:search-algorithm-description ,$1))
               ((algorithm-description steady-state-algorithm-description)
                `(:search-algorithm-description ,$1))
               ((generational-algorithm-description :open :algorithm-generational 
                                                    population-size
                                                    max-generations
                                                    selection-method-description
                                                    elite-manager-description
                                                    :close)
                `((:algorithm-generational MAKE-ALG-GG) ,$3 ,$4 ,$5 ,$6))
               ((steady-state-algorithm-description :open :algorithm-steady-state 
                                                    population-size
                                                    max-iterations
                                                    selection-method-description
                                                    replacement-method-description
                                                    :close)
                `((:algorithm-steady-state MAKE-ALG-SS) ,$3 ,$4 ,$5 ,$6))
               ((max-generations :constant)
                `,$1)
               ((max-iterations :constant)
                `,$1)
               ((population-size :constant)
                `,$1)
               ;; Selection / relacement methods
               ((selection-method-description tournament-selection-method-description)
                `(:selection-method-description ,$1))
               ((selection-method-description ranking-selection-method-description)
                `(:selection-method-description ,$1))
               ((selection-method-description index-selection-method-description)
                `(:selection-method-description ,$1))
               ((selection-method-description random-selection-method-description)
                `(:selection-method-description ,$1))
               ((selection-method-description best-selection-method-description)
                `(:selection-method-description ,$1))
               ((selection-method-description worst-selection-method-description)
                `(:selection-method-description ,$1))
               ((selection-method-description inverse-ranking-selection-method-description)
                `(:selection-method-description ,$1))
               ((selection-method-description inverse-index-selection-method-description)
                `(:selection-method-description ,$1))
               ((replacement-method-description selection-method-description)
                `,$1)
               ((tournament-selection-method-description :open :tournament-selection-method :constant :close)
                `((:tournament-selection-method MAKE-SM-TOURNAMENT) ,$3))
               ((ranking-selection-method-description :open :ranking-selection-method :close)
                :tournament-selection-method)
               ((index-selection-method-description :open :index-selection-method :close)
                :index-selection-method)
               ((random-selection-method-description :open :random-selection-method :close)
                :random-selection-method)
               ((best-selection-method-description :open :best-selection-method :close)
                :best-selection-method)
               ((worst-selection-method-description :open :worst-selection-method :close)
                :worst-selection-method)
               ((inverse-ranking-selection-method-description :open :inverse-ranking-selection-method :close)
                :inverse-ranking-selection-method)
               ((inverse-index-selection-method-description :open :inverse-index-selection-method :close)
                :inverse-index-selection-method)           
               ;; Language
               ((language-description language-description-object)
                `,$1)
               ((language-description-object :open :language-object tree-max-size min-constants max-constants :close)
                `((:language-description-object MAKE-LG) (:language-object ,$3 ,$4 ,$5)))
               ;; Constants 
               ((tree-max-size :constant)
                `,$1)
               ((min-constants :constant)
                `,$1)
               ((max-constants :constant)
                `,$1)
               ;; Initial generation method (3 types)
               ((generator-description generator-object)
                `(:generator-description-object ,$1))
               ((generator-description generator-search-task)
                `(:generator-description-search-task ,$1))
               ;; Random generators (INDIVIDUAL)
               ((generator-object :open :generator-random-object :constant :close)
                `((:generator-random-object MAKE-GN-RND) (:auxiliary-parameter 1)))
               ;; Random generators (TASK)
               ((generator-search-task :open :generator-random-search-task :constant :close)
                `((:generator-random-search-task MAKE-GN-RND-ST) (:auxiliary-parameter 1)))
               ((generator-search-task :open :generator-bests-search-task task-description :close)
                `((:generator-bests-search-task MAKE-GN-BESTS-ST) ,$3))
               ;; Fitness evaluators
               ((fitness-evaluator-description fitness-evaluator-description-object)
                `,$1)
               ((fitness-evaluator-description-object :open :fitness-evaluator-object :constant :close)
                `((:fitness-evaluator-object MAKE-FE) ,$3))
               ;; Genetic operators
               ((operator-usage-description operator-usage-description-object)
                `(:operator-usage-object ,$1))
               ((operator-usage-description-object :operator-usage-object)
                `,$1)
               ;; Elite manager
               ((elite-manager-description :open :elite-manager number-of-elites :close)
                `((:elite-manager-description MAKE-EM) (:elite-manager ,$3)))
               ((number-of-elites :constant) 
                $1))))


(defun search-task-grammar-productions ()
  '((start expresion)
    (expresion object)
    (object best-of-task-description)
    (object best-of-tasks-description)
    (best-of-task-description :open best-of-task task-description-description :close)
    (best-of-tasks-description :open best-of-tasks task-description-list :close)
    (task-description-list task-description-list task-description-description)
    (task-description-list task-description-description)
    (task-description-description :open task-description 
                                  builder-description algorithm-description language-description 
                                  generator-description fitness-evaluator-description
                                  :close)
    (builder-description :open iterative-builder iterations-description :close)
    (iterations-description constant)
    (algorithm-description generational-algorithm-description)
    (algorithm-description steady-state-algorithm-description)
    (generational-algorithm-description 
     :open algorithm-generational 
     population-size max-generations selection-method-description elite-manager-description :close)
    (steady-state-algorithm-description 
     :open algorithm-steady-state 
     population-size max-iterations selection-method-description replacement-method-description :close)
    (max-generations constant)
    (max-iterations constant)
    (population-size constant)
    (selection-method-description tournament-selection-method-description)
    (selection-method-description ranking-selection-method-description)
    (selection-method-description index-selection-method-description)
    (selection-method-description random-selection-method-description)
    (selection-method-description best-selection-method-description)
    (selection-method-description worst-selection-method-description)
    (selection-method-description inverse-ranking-selection-method-description)
    (selection-method-description inverse-index-selection-method-description)
    (replacement-method-description selection-method-description)
    (tournament-selection-method-description :open tournament-selection-method constant :close)
    (ranking-selection-method-description :open ranking-selection-method :close)
    (index-selection-method-description :open index-selection-method :close)
    (random-selection-method-description :open random-selection-method :close)
    (best-selection-method-description :open best-selection-method :close)
    (worst-selection-method-description :open worst-selection-method :close)
    (inverse-ranking-selection-method-description :open inverse-ranking-selection-method :close)
    (inverse-index-selection-method-description :open inverse-index-selection-method :close)
    (language-description language-description-object)
    (language-description-object :open language-object tree-max-size min-constants max-constants :close)
    (tree-max-size constant)
    (min-constants constant)
    (max-constants constant)
    (generator-description generator-object)
    (generator-object :open generator-random-object constant :close)
    (generator-object :open generator-bests-search-task task-description-description :close)
    (generator-object :open generator-random-search-task task-description-description :close)
    (fitness-evaluator-description fitness-evaluator-description-object)
    (fitness-evaluator-description-object :open fitness-evaluator-object constant :close)
    (operator-usage-description operator-usage-description-object)         
    (operator-usage-description-object operator-usage-object)
    (elite-manager-description :open elite-manager number-of-elites :close)
    (number-of-elites constant)
    (constant :constant)))

;;; GLOBAL BINDINGS
(defparameter *default-template-task* nil)
(defparameter *default-template-iteration-builder* nil)

(defparameter *default-template-fitness-evaluator-object* nil)
(defparameter *default-template-generator-random-object* nil)
(defparameter *default-template-generator-bests-object* nil)

(defparameter *default-template-language-object* nil)
(defparameter *default-template-language-operator* nil)
(defparameter *default-template-language-search-task* nil)

(defparameter *default-template-generational-algorithm* nil)
(defparameter *default-template-steady-state-algorithm* nil)

(defparameter *default-template-elite-manager* nil)

(defparameter *default-template-selection-method-tournament* nil)
(defparameter *default-template-selection-method-index* nil)
(defparameter *default-template-selection-method-index-inverse* nil)
(defparameter *default-template-selection-method-ranking* nil)
(defparameter *default-template-selection-method-ranking-inverse* nil)
(defparameter *default-template-selection-method-best* nil)
(defparameter *default-template-selection-method-random* nil)
(defparameter *default-template-selection-method-worst* nil)


(defun initialize-default-search-task-object-templates ()
  ;; Initialize task default objects
  (setf *default-template-task* (make-instance 'search-task))
  (setf *default-template-iteration-builder* (make-instance 'n-runs-task-builder :runs 1))
  ;; Initialize default generic objects
  (setf *default-template-fitness-evaluator-search-task* (system-get 'default-search-task-objetive-fitness-evaluator))
  (setf *default-template-fitness-evaluator-operator* nil)
  (setf *default-template-generator-random-object* (system-get 'random-trees-initializer))
  (setf *default-template-generator-bests-object* (system-get 'task-best-objects-initializer))
  ;; Initialize default algorithms
  (setf *default-template-generational-algorithm* (make-instance 'generational-algorithm :description "default generational"))
  (setf *default-template-steady-state-algorithm* (make-instance 'steady-state-algorithm :description "default steady state"))
  ;; Initialize algorithm default objects
  (setf *default-template-elite-manager* (make-instance 'elite-manager :max-size 1))
  (setf *default-template-selection-method-tournament* (system-get 'tournament-selection-method))
  (setf *default-template-selection-method-index* (system-get 'ranking-proportionate-selection-method))
  (setf *default-template-selection-method-index-inverse* (system-get 'ranking-inverse-proportionate-selection-method))
  (setf *default-template-selection-method-ranking* (system-get 'ranking-inverse-proportionate-selection-method))
  (setf *default-template-selection-method-ranking-inverse* (system-get 'ranking-inverse-proportionate-selection-method))
  (setf *default-template-selection-method-best* (system-get 'best-fitness-selection-method))
  (setf *default-template-selection-method-random* (system-get 'random-selection-method))
  (setf *default-template-selection-method-worst* (system-get 'worst-fitness-selection-method)))


;; Handle some global constraints
(defparameter *min-builder-iterations* 1)
(defparameter *max-builder-iterations* 5)

(defparameter *min-population-size* 3)
(defparameter *max-population-size* 100)

(defparameter *min-generations* 3)
(defparameter *max-generations* 100)

(defparameter *min-iterations* 3)
(defparameter *max-iterations* 100)

(defparameter *min-size-elites* 0)
(defparameter *max-size-elites* *max-population-size*)

(defparameter *min-size-language* 10)
(defparameter *max-size-language* 100)

(defparameter *min-size-constants* 2)
(defparameter *max-size-constants* 1000)


(defun crop-for-property (object value name min max)
  (let ((property (property-named object name)))
    (unless property (error "Property not found"))
    (crop (crop value (min-value property) (max-value property)) min max)))

;; DSL Functions
(defun BEST-OF-TASK (context task)
  (execute-search task) 
  (multiple-value-bind (result subtask) 
      (best-individual task)
    (values result subtask)))

(defun BEST-OF-TASKS (context &rest tasks)
  (execute-search (first tasks))
  (let* ((best-task (first tasks))
         (best (best-individual best-task)))
    (dolist (i (cdr tasks))
      (execute-search i) 
      (multiple-value-bind (result subtask) 
          (best-individual i)
        (when (better-than result best)
          (setf best-task i
                best result))))
    (values best best-task)))

(defun MAKE-TASK (context builder algorithm language generator fitness-evaluator)
  (let ((instance (copy-cyclic *default-template-task*)))
    (setf 
     (parent instance) context
     (algorithm instance) algorithm
     (language instance) language
     (input instance) generator
     (initialization-method algorithm) generator
     (task-builder instance) builder
     (fitness-evaluator instance) fitness-evaluator)
    (make-task-representation-corrections instance)
    instance))

(defun make-task-representation-corrections (task)
  (make-task-representation-corrections-generator task (initialization-method (algorithm task))))

;; Tree generator size correction
(defmethod make-task-representation-corrections-generator (task (o t))
  nil)

(defmethod make-task-representation-corrections-generator (task (o random-trees-generator))
  (setf (min-size o) (min-length (language task))
        (max-size o) (max-size (language task))
        (min-depth o) (min-depth (language task))
        (max-depth o) (max-depth (language task))))

;; Constants correction
(defmethod make-task-representation-corrections-constants (task (o ephemeral-random-constants-factory) min max)
  (setf (min-value o) min
        (max-value o) max))

(defmethod make-task-representation-corrections-constants (task (o fixed-set-constants-factory) min max)
  (setf (constants-set o) 
        (select (constants-set o) 
                (lambda (x) 
                  (between 
                   x 
                   min
                   max)))))

(defun MAKE-BUILDER-IT (context iterations)
  (let ((instance (copy-cyclic *default-template-iteration-builder*)))
    (setf (runs instance) 
          (crop-for-property instance iterations 'runs *min-builder-iterations* *max-builder-iterations*))
    instance))

(defun MAKE-ALG-GG (context population-size max-generations selection-method elite-manager)
  (let ((instance (copy-cyclic *default-template-generational-algorithm*)))
    (setf (population-size instance) (crop-for-property instance population-size 'population-size *min-population-size* *max-population-size*) 
          (max-generations instance) (crop-for-property instance max-generations 'max-generations *min-generations* *max-generations*)
          (selection-method instance) selection-method
          (elite-manager instance) elite-manager)
    instance))

(defun MAKE-ALG-SS (context population-size max-iterations selection-method replacement-method)
  (let ((instance (copy-cyclic *default-template-steady-state-algorithm*)))
    (setf (population-size instance) (crop-for-property instance population-size 'population-size *min-population-size* *max-population-size*)
          (max-iterations instance) (crop-for-property instance max-iterations 'max-iterations *min-iterations* *max-iterations*)
          (selection-method instance) selection-method
          (replacement-strategy instance) replacement-method)
    instance))

(defun MAKE-SM-TOURNAMENT (context tournament-size)
  (copy-cyclic *default-template-selection-method-tournament*))

(defun MAKE-SM-RANK (context)
  (copy-cyclic *default-template-selection-method-tournament*))

(defun MAKE-SM-INDEX (context)
  (copy-cyclic *default-template-selection-method-index*))

(defun MAKE-SM-RANDOM (context)
  (copy-cyclic *default-template-selection-method-random*))

(defun MAKE-SM-BEST (context)
  (copy-cyclic *default-template-selection-method-best*))

(defun MAKE-SM-WORST (context) 
  (copy-cyclic *default-template-selection-method-worst*))

(defun MAKE-SM-IINDEX (context)
  (copy-cyclic *default-template-selection-method-index-inverse*))

(defun MAKE-SM-IRANKING (context)
  (copy-cyclic *default-template-selection-method-ranking-inverse*))

(defun MAKE-EM (context elites-count)
  (let ((instance (copy-cyclic *default-template-elite-manager*)))
    (setf (max-size instance) (crop-for-property instance elites-count 'max-size *min-size-elites* *max-size-elites*))
    instance))

(defun MAKE-LG (context max-size min-constants max-constants)
  (let ((value (copy-cyclic (default-language (make-instance (objetive-class context)))))
        (min (crop (min min-constants max-constants) *min-size-constants* *max-size-constants*))
        (max (crop (max min-constants max-constants) *min-size-constants* *max-size-constants*)))
    (when (< (- max min) 2)
      (incf max)
      (decf min))
    (setf (max-size value) (crop-for-property value max-size 'max-size *min-size-language* *max-size-language*))
    (make-task-representation-corrections-constants value (constants-strategy value) min max)
    value))

(defun MAKE-OBJ (context program)
  (declare (ignore context))
  (make-instance (objetive-class task) :expression program))

(defun MAKE-GN-RND (context aux)
  (declare (ignore aux context))
  (copy-cyclic *default-template-generator-random-object*))

(defun MAKE-GN-RND-ST (context input-task)
  (declare (ignore context))
  (let ((instance (copy-cyclic *default-template-generator-bests-object*)))
    (execute-search input-task)
    (setf (process input-task) nil
          (input-task instance) input-task)
    instance))

(defun MAKE-GN-BESTS-ST (context input-task)
  (declare (ignore context))
  (let ((instance (copy-cyclic *default-template-generator-bests-object*)))
    (execute-search input-task)
    (setf (process input-task) nil
          (input-task instance) input-task)
    instance))

(defun MAKE-FE (context auxiliar)
  (declare (ignore auxiliar))
  (copy-cyclic (first (default-fitness-evaluators (make-instance (objetive-class context))))))
