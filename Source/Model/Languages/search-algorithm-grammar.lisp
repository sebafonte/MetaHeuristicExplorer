
;;
;; INCLUDE ELITE MANAGER
;; CHECK ITERATOR OBJECTS AND REFACTOR IF NECESSARY
;; ADD OTHER OBJECTS TO EXTEND LANGUAGE
;;

(defclass search-algorithm-grammar (context-free-grammar)
  ())

;; Language dependent symbols are defined in each language grammar
(defparameter *search-algorithm-grammar-tokens*
  '((make-evolutionary-algorithm-description :make-evolutionary-algorithm-description)
    (make-generational-evolver :make-generational-evolver)
    (make-iterational-evolver :make-iterational-evolver)
    (make-binary-operation-description :make-binary-operation-description)
    (make-unary-operation-description :make-unary-operation-description)
    (make-population-initializer :make-population-initializer)
    (make-grow-individual-generator :make-grow-individual-generator)
    (make-half-and-half-generator :make-half-and-half-generator)
    (make-random-generator :make-random-generator)
    (make-iterator-description :make-iterator-description)
    (make-tree-language :make-tree-language)
    (make-elite-manager :make-elite-manager)
    (initializer-operation :initializer-operation)
    (make-selection-method-description :make-selection-method-description)
    (make-specific-operation :make-specific-operation)
    (:operation-description :operation-description)
    (:list-auxiliar :list-auxiliar)
    (:language :language)
    (:operations :operations)
    (:operation :operation)
    (:child-iterations :child-iterations)
    (:max-generations :max-generations)
    (:max-iterations :max-iterations)
    (:elite-manager :elite-manager)
    (:selection-function :selection-function)
    (:selection-function-a :selection-function-a)
    (:selection-function-b :selection-function-b)
    (:node-weight-function :node-weight-function)
    (:population-size :population-size)
    (:weight :weight)
    (:name :name)
    (:min-size :min-size)
    (:max-size :max-size)
    (:max-depth :max-depth)
    (:initializer :initializer)
    (:evolver :evolver)
    (:iterator :iterator)
    (:open :open)
    (:close :close)
    (:node-weight-function :node-weight-function)
    (:node-weight-function-a :node-weight-function-a)
    (:node-weight-function-b :node-weight-function-b)
    (:default-node-weight :node-weight-method-name)
    (:tournament-selection-method :selection-method-name)
    (:random-selection-method :selection-method-name)))

(defun search-algorithm-grammar-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol
        (search-algorithm-grammar-get-token grammar symbol)
      symbol)))

(defun search-algorithm-grammar-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (if (equal token-type :unknown) 
        (setf token-type 
              (if (numberp word) :constant 
                (if (listp word) :list
                  (if (stringp word) :string
                    (if (symbolp word) :symbol))))))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-search-algorithm-grammar-parser (name)
  (search-algorithm-grammar-parser name))

(defun search-algorithm-grammar-parser (name)
  (eval
   `(defparser ,name
               ;; Start
               ((start algorithm-description) 
                $1)
               ;; Algorithm description
               ((algorithm-description 
                 :open
                 :make-evolutionary-algorithm-description 
                 :name :string
                 :language language-description
                 :population-size :constant
                 :initializer initializer-description
                 :evolver evolver-description
                 :elite-manager elite-manager-description
                 :close)
                `((:algorithm-description MAKE-EVOLUTIONARY-ALGORITHM-DESCRIPTION) 
                  (:name :name) ,$4 
                  (:language :language) ,$6 
                  (:population-size :population-size) ,$8 
                  (:initializer :initializer) ,$10 
                  (:evolver :evolver) ,$12 
                  (:elite-manager :elite-manager) ,$14))
               ;; Language description
               ((language-description 
                 :open :make-tree-language
                 :name :symbol
                 :max-size :constant
                 :close)
                `((:initializer-description MAKE-TREE-LANGUAGE) 
                  (:name :name) ,$4 
                  (:max-size :max-size) ,$6))
               ;; Initializer description
               ((initializer-description 
                 :open :make-population-initializer 
                 :open :list-auxiliar initializer-operations-description :close 
                 :close)
                `((:initializer-description MAKE-POPULATION-INITIALIZER) 
                  ((:list-auxiliar :list-auxiliar) ,$5)))
               ;; Evolver description
               ((evolver-description make-generational-evolver-description)
                `(:evolver-description ,$1))
               ((evolver-description make-iterational-evolver-description)
                `(:evolver-description ,$1))
               ((make-generational-evolver-description 
                 :open 
                 :make-generational-evolver 
                 :max-generations :constant 
                 :child-iterations :constant 
                 :operations :open :list-auxiliar operations-description :close
                 :iterator iterator-description
                 :close)
                `((:iterational-evolver-description MAKE-GENERATIONAL-EVOLVER) 
                  (:max-generations :max-generations) ,$4 
                  (:child-iterations :child-iterations) ,$6 
                  (:operations :operations) ((:list-auxiliar :list-auxiliar) ,$10)
                  (:iterator :iterator) ,$13))
               ((make-iterational-evolver-description
                 :open 
                 :make-iterational-evolver 
                 :max-iterations :constant 
                 :child-iterations :constant
                 :operations :open :list-auxiliar operations-description :close
                 :iterator iterator-description
                 :close)
                `((:generational-evolver-description MAKE-ITERATIONAL-EVOLVER) 
                  (:max-iterations :max-iterations) ,$4 
                  (:child-iterations :child-iterations) ,$6 
                  (:operations :operations) ((:list-auxiliar :list-auxiliar) ,$9)
                  (:iterator :iterator) ,$12))
               ;; Initializer operations description
               ((initializer-operations-description 
                 initializer-operations-description initializer-operation-description)
                `(:initializer-operations-descriptions ,$1 ,$2))
               ((initializer-operations-description initializer-operation-description)
                `(:initializer-operations-description ,$1))
               ((initializer-operation-description
                 :open :make-half-and-half-generator :max-depth :constant :weight :constant :close)
                `((:initializer-operation-description MAKE-HALF-AND-HALF-GENERATOR) 
                  :max-depth ,$4 :weight ,$6))
               ((initializer-operation-description 
                 :open 
                 :make-grow-individual-generator 
                 :max-size :constant 
                 :max-depth :constant 
                 :weight :constant 
                 :close)
                `((:initializer-operation-description MAKE-GROW-INDIVIDUAL-GENERATOR) 
                  (:max-size :max-size) ,$4 
                  (:max-depth :max-depth) ,$6 
                  (:weight :weight) ,$8))
               ((initializer-operation-description 
                 :open :make-random-generator :min-size :constant :max-size :constant :weight :constant :close)
                `((:initializer-operation-description MAKE-RANDOM-INDIVIDUAL-GENERATOR) 
                  (:min-size :min-size) ,$4 
                  (:max-size :max-size) ,$6
                  (:weight :weight) ,$8))
               ;; Evolver iterator description
               ((iterator-description 
                 :open :make-iterator-description :open :list-auxiliar iterator-operations :close :close)
                `((:iterator-description MAKE-ITERATOR-DESCRIPTION) 
                  ((:list-auxiliar :list-auxiliar) ,$5)))
               ((iterator-operations iterator-operations operation-specific)
                `(:iterator-operations ,$1 ,$2))
               ((iterator-operations operation-specific)
                `(:iterator-operations ,$1))
               ;; Elite manager description
               ((elite-manager-description 
                 :open 
                 :make-elite-manager 
                 :max-size :constant 
                 :close)
                `((:elite-manager MAKE-ELITE-MANAGER) 
                  (:max-size :max-size) ,$4))
               ;; Operations description
               ((operations-description operations-description operation-description)
                `(:operations-descriptions ,$1 ,$2))
               ((operations-description operation-description)
                `(:operations-descriptions ,$1))
               ((operation-description 
                 :open 
                 :make-unary-operation-description 
                 :operation :unary-operation-name 
                 :selection-function selection-method-description
                 :node-weight-function :node-weight-method-name
                 :weight :constant
                 :close)
                `((:operation-description MAKE-UNARY-OPERATION-DESCRIPTION) 
                  (:operation :operation) ,$4 
                  (:selection-function :selection-function) ,$6 
                  (:node-weight-function :node-weight-function) ,$8 
                  (:weight :weight) ,$10))
               ((operation-description 
                 :open 
                 :make-binary-operation-description 
                 :operation :binary-operation-name
                 :selection-function-a selection-method-description
                 :selection-function-b selection-method-description
                 :node-weight-function-a :node-weight-method-name
                 :node-weight-function-b :node-weight-method-name
                 :weight :constant
                 :close)
                `((:operation-description MAKE-BINARY-OPERATION-DESCRIPTION)
                  (:operation :operation) ,$4
                  (:selection-function-a :selection-function-a) ,$6 
                  (:selection-function-b :selection-function-b) ,$8 
                  (:node-weight-function-a :node-weight-function-a) ,$10
                  (:node-weight-function-b :node-weight-function-b) ,$12 
                  (:weight :weight) ,$14))
               ;; Selection / replacement methods
               ((selection-method-description :open :make-selection-method-description :selection-method-name :close)
                `((:selection-method-description MAKE-SELECTION-METHOD-DESCRIPTION) ,$3))
               ;; Specific functions
               ((operation-specific :open :make-specific-operation :symbol :close)
                `((:specific-operation MAKE-SPECIFIC-OPERATION) ,$3)))))


(defun search-algorithm-grammar-productions ()
  '((start algorithm-description)
    (algorithm-description :open
                           :make-evolutionary-algorithm-description 
                           :name :string
                           :language language-description
                           :population-size :constant
                           :initializer initializer-description
                           :evolver evolver-description
                           :elite-manager elite-manager-description
                           :close)
    (language-description :open :make-tree-language
                          :name :symbol
                          :max-size :constant
                          :close)
    (initializer-description :open :make-population-initializer 
                             :open :list-auxiliar initializer-operations-description :close :close)
    (evolver-description make-generational-evolver-description)
    (evolver-description make-iterational-evolver-description)
    (make-generational-evolver-description :open :make-generational-evolver
                                           :max-generations :constant 
                                           :child-iterations :constant 
                                           :operations :open :list-auxiliar operations-description :close 
                                           :iterator iterator-description
                                           :close)
    (make-iterational-evolver-description :open :make-iterational-evolver 
                                          :max-iterations :constant :child-iterations :constant 
                                          :operations :open :list-auxiliar operations-description :close
                                          :iterator iterator-description :close)
    (iterator-description :open :make-iterator-description :open iterator-operations :close :close)
    (iterator-operations iterator-operations operation-specific)
    (iterator-operations operation-specific)
    (initializer-operations-description initializer-operations-description initializer-operation-description)
    (initializer-operations-description initializer-operation-description)
    (initializer-operation-description :open :make-half-and-half-generator :max-size :constant :weight :constant :close)
    (initializer-operation-description :open :make-grow-individual-generator :max-size :constant :close)
    (initializer-operation-description :open :make-random-generator :min-size :constant :max-size :constant 
                                       :weight :constant :close)
    (iterator-description :open :make-iterator-description :open :list-auxiliar iterator-operations :close :close)
    (iterator-operations iterator-operations operation-specific)
    (iterator-operations operation-specific)
    (elite-manager-description :open :make-elite-manager :max-size :constant :close)
    (operations-description :open operations-description operation-description :close)
    (operations-description :operation-description operation-description)
    (operation-description :open :make-unary-operation-description 
                           :operation :unary-operation-name
                           :selection-function selection-method-description
                           :node-weight-function :node-weight-method-name
                           :weight :constant
                           :close)
    (operation-description :open :make-binary-operation-description 
                           :operation :binary-operation-name
                           :selection-function-a selection-method-description
                           :selection-function-b selection-method-description
                           :node-weight-function-a :node-weight-method-name
                           :node-weight-function-b :node-weight-method-name
                           :weight :constant
                           :close)
    (selection-method-description :open :make-selection-method-description :selection-method-name :close)
    (operation-specific :open :make-specific-operation :symbol :close)))


;; Algorithms
(defun MAKE-EVOLUTIONARY-ALGORITHM-DESCRIPTION (&key name language population-size initializer evolver elite-manager)
  (make-instance 'configurable-search-algorithm
                 :name name
                 :description name
                 :language language
                 :population-size population-size
                 :initializer initializer
                 :evolver evolver
                 :elite-manager elite-manager))

;; Algorithm parts
(defun MAKE-EVOLVER (population-size iterations selection-method replacement-method)
  (error "Subclass responsibility"))

(defun MAKE-FITNESS-EVALUATOR (name)
  (system-get-copy name))

(defun MAKE-TREE-LANGUAGE (&key name max-size)
  (let ((instance (system-get-copy (intern name))))
    (setf (max-size instance) max-size)
    instance))

;; Selection methods
(defun MAKE-SELECTION-METHOD-DESCRIPTION (name)
  (system-get-copy (intern name)))

;; Initializer operations
(defun MAKE-HALF-AND-HALF-GENERATOR (&key max-depth weight)
  (make-instance 'unary-operation-description
                 :operation (make-instance 'ramped-half-and-half-generator
                                           :min-value 1
                                           :max-value max-depth)
                 :weight weight))

(defun MAKE-GROW-INDIVIDUAL-GENERATOR (&key max-depth max-size weight)
  (make-instance 'unary-operation-description
                 :operation (make-instance 'random-trees-generator
                                           :min-size max-size
                                           :max-size max-size
                                           :min-depth max-depth
                                           :max-depth max-depth)
                 :weight weight))

(defun MAKE-RANDOM-INDIVIDUAL-GENERATOR (&key max-depth max-size weight)
  (make-instance 'unary-operation-description
                 :operation (make-instance 'random-trees-generator
                                           :min-size 1
                                           :max-size max-size
                                           :min-depth 1
                                           :max-depth max-depth)
                 :weight weight))

;; Operations
(defun MAKE-UNARY-OPERATION-DESCRIPTION (&key operation selection-function node-weight-function weight)
  (make-instance 'unary-operation-description 
                 :operation (system-get-copy (intern operation))
                 :node-weight-function node-weight-function
                 :selection-function selection-function
                 :weight weight))

(defun MAKE-BINARY-OPERATION-DESCRIPTION (&key operation weight selection-function-a selection-function-b 
                                               node-weight-function-a node-weight-function-b)
  (make-instance 'binary-operation-description
                 :operation (system-get-copy (intern operation))
                 :selection-a-function selection-function-a
                 :selection-b-function selection-function-b
                 :node-weight-function-a node-weight-function-a
                 :node-weight-function-b node-weight-function-b
                 :weight weight))

;; Algorithm specific
(defun MAKE-SPECIFIC-OPERATION (name)
  (make-instance 'operation-wrapper :operation (intern name)))

(defun MAKE-ELITE-MANAGER (&key max-size)
  (make-instance 'elite-manager :max-size max-size))

(defmacro MAKE-GENERATIONAL-EVOLVER (&key max-generations child-iterations operations iterator)
  `(make-instance 'generational-evolver
                  :max-generations ,max-generations
                  :child-iterations ,child-iterations
                  :operations (list ,@operations)
                  :iterator ,iterator))

(defmacro MAKE-ITERATIONAL-EVOLVER (&key max-iterations child-iterations operations iterator)
  `(make-instance 'iterational-evolver
                  :max-iterations ,max-iterations
                  :child-iterations ,child-iterations
                  :operations (list ,@operations)
                  :iterator ,iterator))

(defmacro MAKE-ITERATOR-DESCRIPTION (operations)
  `(make-instance 'evolver-iterator
                  :operations (list ,@operations)))

(defmacro MAKE-POPULATION-INITIALIZER (operations)
  `(make-instance 'combined-population-generator
                  :operations (list ,@operations)))

 