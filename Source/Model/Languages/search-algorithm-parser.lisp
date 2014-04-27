
(defclass search-algorithm-grammar (context-free-grammar)
  ())


(defparameter *search-task-grammar-tokens*
  '(;; Objects
    (OBJ :search-object)
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
    (MAKE-GN-RND-OBJ :generator-random-object)
    (MAKE-GN-RND-OP :generator-random-operator)
    (MAKE-GN-RND-ST :generator-random-search-task)
    (MAKE-GN-BESTS-OBJ :generator-bests-object)
    (MAKE-GN-BESTS-OP :generator-bests-operator)
    (MAKE-GN-BESTS-ST :generator-bests-search-task)
    ;; Languages
    (MAKE-LG-OBJ :language-object)
    (MAKE-LG-OP :language-operator)
    (MAKE-LG-ST :language-search-task)
    ;; Language functions
    (MAKE-LGF-OBJ :language-functions-object)
    (MAKE-LGF-ST :language-functions-search-task)
    (MAKE-LGF-OP :language-functions-operator)
    ;; Fitness evaluators
    (MAKE-FE-OBJ :fitness-evaluator-object)
    (MAKE-FE-OP :fitness-evaluator-operator)
    (MAKE-FE-ST :fitness-evaluator-search-task)
    ;; Operators usage
    (MAKE-OP-OBJ :operator-description-object)
    (MAKE-OP-ST :operator-description-search-task)
    (MAKE-OP-OP :operator-description-operator)
    ;; Operators usage
    (MAKE-OPU-OBJ :operator-usage-object)
    (MAKE-OPU-ST :operator-usage-search-task)
    (MAKE-OPU-OP :operator-usage-operator)
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
  "Answer the token type of word."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (if (equal token-type :unknown) 
        (setf token-type 
              (if (numberp word) :constant 
                (if (listp word) :list))))
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
               ((object :search-object)
                `(:search-object-description ,$1))
               ((object best-of-task-description)
                `(:search-object-description ,$1))
               ((object best-of-tasks-description)
                `(:search-object-description ,$1))
               ;; Search tasks
               ((best-of-task-description :open :best-of-task task-description :close)
                `((:best-of-task BEST-OF-TASK) ,$3))
               ((best-of-tasks-description :open :best-of-tasks task-description-list :close)
                `((:best-of-tasks BEST-OF-TASKS) ,$3))
               ((task-description-list task-description-list task-description)
                `(:task-description-list ,$1 ,$2))
               ((task-description-list task-description)
                `(:task-description-list ,$1))
               ((task-description :open :task-description 
                                  builder-description algorithm-description language-description 
                                  generator-description fitness-evaluator-description
                                  :close)
                `((:task-description MAKE-TASK) ,$3 ,$4 ,$5 ,$6 ,$7))
               ;; Task builder: #TODO: UN-HARDCODE this iterative builder!
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
               ;; language (3 types)
               ((language-description language-description-object)
                `,$1)
               ((language-description language-description-search-task)
                `,$1)
               ((language-description language-description-operator)
                `,$1)
               ((language-description-object :open :language-object
                                             tree-max-size
                                             min-constants
                                             max-constants
                                             :close)
                `((:language-description-object MAKE-LG-OBJ) (:language-object ,$3 ,$4 ,$5)))
               ((language-description-search-task :open :language-search-task
                                                  tree-max-size
                                                  min-constants
                                                  max-constants
                                                  :close)
                `((:language-description-search-task MAKE-LG-ST) (:language-search-task ,$3 ,$4 ,$5)))
               ((language-description-operator :open :language-operator
                                               tree-max-size
                                               min-constants
                                               max-constants
                                               :close)
                `((:language-description-operator MAKE-LG-OP) (:language-operator ,$3 ,$4 ,$5)))
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
               ((generator-description generator-operator)
                `(:generator-description-operator ,$1))
               ;; Random generators
               ((generator-object :open :generator-random-object :constant :close)
                `((:generator-random-object MAKE-GN-RND-OBJ) (:auxiliary-parameter 1)))
               ((generator-search-task :open :generator-random-search-task :constant :close)
                `((:generator-random-search-task MAKE-GN-RND-ST) (:auxiliary-parameter 1)))
               ((generator-operator :open :generator-random-operator :constant :close)
                `((:generator-random-operator MAKE-GN-RND-OP) (:auxiliary-parameter 1)))
               ;; Best objects from task generators
               ((generator-object :open :generator-bests-object task-description :close)
                `((:generator-bests-object MAKE-GN-BESTS-OBJ) ,$3))
               ((generator-search-task :open :generator-bests-search-task task-description :close)
                `((:generator-bests-search-task MAKE-GN-BESTS-ST) ,$3))
               ((generator-operator :open :generator-bests-operator task-description :close)
                `((:generator-bests-operator MAKE-GN-BESTS-OP) ,$3))
               ;; Fitness evaluators (3 types)
               ((fitness-evaluator-description fitness-evaluator-description-object)
                `,$1)
               ((fitness-evaluator-description fitness-evaluator-description-search-task)
                `,$1)
               ((fitness-evaluator-description fitness-evaluator-description-operator)
                `,$1)
               ((fitness-evaluator-description-object :open :fitness-evaluator-object :constant :close)
                `((:fitness-evaluator-object MAKE-FE-OBJ) ,$3))
               ((fitness-evaluator-description-search-task :open :fitness-evaluator-search-task :constant :close)
                `((:fitness-evaluator-search-task MAKE-FE-ST) ,$3))
               ((fitness-evaluator-description-operator :open :fitness-evaluator-operator :constant :close)
                `((:fitness-evaluator-operator MAKE-FE-OP) ,$3))
               ;; Genetic operators (3 types)
               ((operator-usage-description operator-usage-description-object)
                `(:operator-usage-object ,$1))
               ((operator-usage-description operator-usage-description-search-task)
                `(:operator-usage-search-task ,$1))
               ((operator-usage-description operator-usage-description-operator)
                `(:operator-usage-operator ,$1))
               ((operator-usage-description-object :operator-usage-object)
                `,$1)
               ((operator-usage-description-search-task :operator-usage-search-task)
                `,$1)
               ((operator-usage-description-operator :operator-usage-operator)
                `,$1)
               ;; Elite manager
               ((elite-manager-description :open :elite-manager number-of-elites :close)
                `((:elite-manager-description MAKE-EM) (:elite-manager ,$3)))
               ((number-of-elites :constant) 
                $1))))


(defun search-task-grammar-productions ()
  '((start expresion)
    (expresion object)
    (object :search-object)
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
    (language-description language-description-search-task)
    (language-description language-description-operator)
    (language-description-object :open language-object tree-max-size min-constants max-constants :close)
    (language-description-search-task :open language-search-task tree-max-size min-constants max-constants :close)
    (language-description-operator :open language-operator tree-max-size min-constants max-constants :close)
    (tree-max-size constant)
    (min-constants constant)
    (max-constants constant)
    (generator-description generator-object)
    (generator-description generator-search-task)
    (generator-description generator-operator)
    (generator-object :open generator-random-object constant :close)
    (generator-search-task :open generator-random-search-task constant :close)
    (generator-operator :open generator-random-operator constant :close)
    (generator-object :open generator-bests-object task-description-description :close)
    (generator-search-task :open generator-bests-search-task task-description-description :close)
    (generator-operator :open generator-bests-operator task-description-description :close)
    (fitness-evaluator-description fitness-evaluator-description-object)
    (fitness-evaluator-description fitness-evaluator-description-search-task)
    (fitness-evaluator-description fitness-evaluator-description-operator)
    (fitness-evaluator-description-object :open fitness-evaluator-object constant :close)
    (fitness-evaluator-description-search-task :open fitness-evaluator-search-task constant :close)
    (fitness-evaluator-description-operator :open fitness-evaluator-operator constant :close)
    (operator-usage-description operator-usage-description-object)         
    (operator-usage-description operator-usage-description-search-task)
    (operator-usage-description operator-usage-description-operator)
    (operator-usage-description-object operator-usage-object)
    (operator-usage-description-search-task operator-usage-search-task)
    (operator-usage-description-operator operator-usage-operator)
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
  ;; Initialize typed objects
  (setf *default-template-fitness-evaluator-object* (system-get 'medium-0-10-function-x-y-evaluator))
  (setf *default-template-fitness-evaluator-search-task* (system-get 'default-search-task-objetive-fitness-evaluator))
  (setf *default-template-fitness-evaluator-operator* nil)
  (setf *default-template-generator-random-object* (system-get 'random-trees-initializer))
  (setf *default-template-generator-bests-object* (system-get 'task-best-objects-initializer))
  (setf *default-template-language-object* (system-get 'lisp-math-function-xy))
  (setf *default-template-language-operator* nil)
  ;; Initialize default algorithms
  (setf *default-template-generational-algorithm* (make-instance 'generational-algorithm))
  (setf *default-template-steady-state-algorithm* (make-instance 'steady-state-algorithm))
  ;; Initialize algorithm default objects
  (setf *default-template-elite-manager* (make-instance 'elite-manager :max-size 1))
  (setf *default-template-selection-method-tournament* (system-get 'tournament-selection-method))
  (setf *default-template-selection-method-index* (system-get 'ranking-proportionate-selection-method))
  (setf *default-template-selection-method-index-inverse* (system-get 'ranking-inverse-proportionate-selection-method))
  (setf *default-template-selection-method-ranking* (system-get 'ranking-inverse-proportionate-selection-method))
  (setf *default-template-selection-method-ranking-inverse* 
        (system-get 'ranking-inverse-proportionate-selection-method))
  (setf *default-template-selection-method-best* (system-get 'best-fitness-selection-method))
  (setf *default-template-selection-method-random* (system-get 'random-selection-method))
  (setf *default-template-selection-method-worst* (system-get 'worst-fitness-selection-method)))


(defun BEST-OF-TASK (task)
  (execute-search task) 
  (multiple-value-bind (result context) 
      (best-individual task)
    result))

(defun BEST-OF-TASKS (&rest tasks)
  (let ((task (first tasks)))
    (execute-search task) 
    (multiple-value-bind (result context) 
        (best-individual task)
      result)))

(defun MAKE-TASK (builder algorithm language generator fitness-evaluator)
  (let ((instance (copy *default-template-task*)))
    (setf (algorithm (context instance)) algorithm
          (language instance) language
          (input instance) generator
          ;; #TODO: Fix and take this out of here
          (initialization-method algorithm) generator
          (task-builder instance) builder
          (fitness-evaluator instance) fitness-evaluator)
    instance))

(defun MAKE-BUILDER-IT (iterations)
  (let ((instance (copy *default-template-iteration-builder*)))
    (setf (runs instance) iterations)
    instance))

(defun MAKE-ALG-GG (population-size generations selection-method elite-manager)
  (let ((instance (copy *default-template-generational-algorithm*)))
    (setf (population-size instance) population-size
          (max-generations instance) generations
          (selection-method instance) selection-method
          (elite-manager instance) elite-manager)
    instance))

(defun MAKE-ALG-SS (population-size iterations selection-method replacement-method)
  (let ((instance (copy *default-template-steady-state-algorithm*)))
    (setf (population-size instance) population-size
          (max-iterations instance) iterations
          (selection-method instance) selection-method
          (replacement-strategy instance) replacement-method)
    instance))

(defun MAKE-SM-TOURNAMENT (tournament-size)
  (copy *default-template-selection-method-tournament*))

(defun MAKE-SM-RANK ()
  (copy *default-template-selection-method-tournament*))

(defun MAKE-SM-INDEX ()
  (copy *default-template-selection-method-index*))

(defun MAKE-SM-RANDOM (tournament-size)
  (copy *default-template-selection-method-random*))

(defun MAKE-SM-BEST ()
  (copy *default-template-selection-method-best*))

(defun MAKE-SM-WORST () 
  (copy *default-template-selection-method-worst*))

(defun MAKE-SM-IINDEX ()
  (copy *default-template-selection-method-index-inverse*))

(defun MAKE-SM-IRANKING ()
  (copy *default-template-selection-method-ranking-inverse*))

(defun MAKE-EM (elites-count)
  (let ((instance (copy *default-template-elite-manager*)))
    (setf (max-size instance) elites-count)
    instance))

(defun MAKE-LG-OBJ (a b c)
  (copy *default-template-language-object*))

(defun MAKE-LG-OP (a b c)
  (copy *default-template-language-operator*))

(defun MAKE-GN-RND-OBJ (auxiliar)
  (copy *default-template-generator-random-object*))

(defun MAKE-GN-BESTS-OBJ (input-task)
  (let ((instance (copy *default-template-generator-bests-object*)))
    (execute-search input-task)
    (setf (proceso input-task) nil)
    (setf (input-task instance) input-task)
    instance))

(defun MAKE-GN-BESTS-ST ()
  (let ((instance (copy *default-template-generator-bests-object*)))
    (execute-search input-task)
    (setf (proceso input-task) nil)
    (setf (input-task instance) input-task)
    instance))

(defun MAKE-FE-OBJ (auxiliar)
  (copy *default-template-fitness-evaluator-object*))

(defun MAKE-FE-ST (auxiliar)
  (copy *default-template-fitness-evaluator-search-task*))

(defun MAKE-FE-OP (auxiliar)
  (copy *default-template-fitness-evaluator-operator*))
