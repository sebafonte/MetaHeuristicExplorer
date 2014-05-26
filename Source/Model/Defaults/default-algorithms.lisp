
(defun initialize-default-algorithms ()
  (system-add 
   ;; Default algorithms
   (make-instance 'registrable-object-wrapper
                  :name 'default-generational-algorithm
                  :subject (make-instance 'generational-algorithm :description "Generational")
                  :description "Generational")
   (make-instance 'registrable-object-wrapper
                  :name 'default-steady-state-algorithm
                  :subject (make-instance 'steady-state-algorithm :description "Steady state")
                  :description "Steady state")
   (make-instance 'registrable-object-wrapper
                  :name 'default-nsga-ii-algorithm
                  :subject (make-instance 'nsga-ii :description "NSGA-II")
                  :description "NSGA-II")
   ;; Composite algorithms
   (make-instance 'registrable-object-wrapper
                  :name 'default-iterational-algorithm-1
                  :subject (make-sample-composite-algorithm (sample-composite-algorithm-code-1))
                  :description "Iterational 1")
   (make-instance 'registrable-object-wrapper
                  :name 'default-iterational-algorithm-2
                  :subject (make-sample-composite-algorithm (sample-composite-algorithm-code-2))
                  :description "Iterational 2")
   (make-instance 'registrable-object-wrapper
                  :name 'default-iterational-algorithm-3
                  :subject (make-sample-composite-algorithm (sample-composite-algorithm-code-3))
                  :description "Iterational 3")
   (make-instance 'registrable-object-wrapper
                  :name 'default-iterational-algorithm-4
                  :subject (make-sample-composite-algorithm (sample-composite-algorithm-code-4))
                  :description "Iterational 4")))

(defun default-search-algorithms ()
  (list (system-get-subject-copy 'default-generational-algorithm)
        (system-get-subject-copy 'default-steady-state-algorithm)
        (system-get-subject-copy 'default-nsga-ii-algorithm)
        (system-get-subject-copy 'default-iterational-algorithm-1)
        (system-get-subject-copy 'default-iterational-algorithm-2)
        (system-get-subject-copy 'default-iterational-algorithm-3)))

(defun make-sample-composite-algorithm (code)
  (let* ((language (system-get 'evolutive-algorithm-language-x-y))
         (grammar (grammar language))
         (reduced-code (compress-flatten-parenthesis-token-value (parse grammar code)))
         (object (eval (replace-label-cadr :list-auxiliar reduced-code))))
    (setf (program object) code)
    object))


(defun sample-composite-algorithm-code-1 ()
  '(make-evolutionary-algorithm-description
    :name "Composite generational 1"
    :language 
    (make-tree-language :name :lisp-math-function-xy :max-size 10)
    :population-size 20
    :initializer 
    (make-population-initializer 
     (:list-auxiliar
      (make-grow-individual-generator :max-size 10 :max-depth 10 :weight 1)))
    :evolver 
    (make-generational-evolver
     :max-generations 100
     :child-iterations 10
     :operations
     (:list-auxiliar
      (make-unary-operation-description
       :operation :mutate-cfg
       :selection-function (make-selection-method-description :tournament-selection-method)
       :node-weight-function :default-node-weight
       :weight 1))
     :iterator
     (make-iterator-description
      (:list-auxiliar
       (make-specific-operation :breed)
       (make-specific-operation :register-elites)
       (make-specific-operation :register-run-data)
       (make-specific-operation :increase-evolution-phase))))
    :elite-manager
    (make-elite-manager :max-size 5)))

(defun sample-composite-algorithm-code-2 ()
  '(make-evolutionary-algorithm-description
    :name "Composite iterational 1"
    :language 
    (make-tree-language :name :lisp-math-function-xy :max-size 10)
    :population-size 20
    :initializer 
    (make-population-initializer 
     (:list-auxiliar
      (make-grow-individual-generator :max-size 10 :max-depth 10 :weight 1)))
    :evolver 
    (make-iterational-evolver
     :max-iterations 10
     :child-iterations 10
     :operations
     (:list-auxiliar
      (make-unary-operation-description
       :operation :mutate-cfg
       :selection-function (make-selection-method-description :tournament-selection-method)
       :node-weight-function :default-node-weight
       :weight 0.5)
      (make-binary-operation-description
       :operation :crossover-cfg
       :selection-function-a (make-selection-method-description :tournament-selection-method)
       :selection-function-b (make-selection-method-description :tournament-selection-method)
       :node-weight-function-a :default-node-weight
       :node-weight-function-b :default-node-weight
       :weight 0.5))
     :iterator
     (make-iterator-description
      (:list-auxiliar
       (make-specific-operation :breed)
       (make-specific-operation :register-elites)
       (make-specific-operation :register-run-data)
       (make-specific-operation :increase-evolution-phase))))
    :elite-manager
    (make-elite-manager :max-size 10)))

(defun sample-composite-algorithm-code-3 ()
  '(make-evolutionary-algorithm-description
    :name "Composite generational 2"
    :language 
    (make-tree-language :name :lisp-math-function-xy :max-size 10)
    :population-size 20
    :initializer 
    (make-population-initializer 
     (:list-auxiliar
      (make-grow-individual-generator :max-size 10 :max-depth 10 :weight 1)))
    :evolver 
    (make-generational-evolver
     :max-generations 20000
     :child-iterations 10
     :operations
     (:list-auxiliar
      (make-unary-operation-description
       :operation :mutate-cfg
       :selection-function (make-selection-method-description :tournament-selection-method)
       :node-weight-function :default-node-weight
       :weight 0.5)
      (make-binary-operation-description
       :operation :crossover-cfg
       :selection-function-a (make-selection-method-description :tournament-selection-method)
       :selection-function-b (make-selection-method-description :tournament-selection-method)
       :node-weight-function-a :default-node-weight
       :node-weight-function-b :default-node-weight
       :weight 0.5))
     :iterator
     (make-iterator-description
      (:list-auxiliar
       (make-specific-operation :breed)
       (make-specific-operation :register-elites)
       (make-specific-operation :register-run-data)
       (make-specific-operation :increase-evolution-phase))))
    :elite-manager
    (make-elite-manager :max-size 10)))



;; #TODO: #CHECK:
;;   - Unique hash table (check usage)
;;   - normalize-and-sort-population-fitness in generational / steady state
;;
;; #TODO: CREATE DEFAULTS:
;;   - Generational algortithm with local optimization
;;   - Evolutionary search base
;;   - Pure random creation approach
;;

(defun sample-composite-algorithm-code-4 ()
  '(make-evolutionary-algorithm-description
    :name "Generational"
    :language 
    (make-tree-language :name :lisp-math-function-xy :max-size 10)
    :population-size 20
    :initializer 
    (make-population-initializer 
     (:list-auxiliar
      (make-grow-individual-generator :max-size 10 :max-depth 10 :weight 1)))
    :evolver 
    (make-generational-evolver
     :max-generations 20000
     :child-iterations 10
     :operations
     (:list-auxiliar
      (make-unary-operation-description
       :operation :mutate-cfg
       :selection-function (make-selection-method-description :tournament-selection-method)
       :node-weight-function :default-node-weight
       :weight 0.5)
      (make-binary-operation-description
       :operation :crossover-cfg
       :selection-function-a (make-selection-method-description :tournament-selection-method)
       :selection-function-b (make-selection-method-description :tournament-selection-method)
       :node-weight-function-a :default-node-weight
       :node-weight-function-b :default-node-weight
       :weight 0.5))
     :iterator
     (make-iterator-description
      (:list-auxiliar
       (make-specific-operation :breed)
       (make-specific-operation :register-elites)
       (make-specific-operation :register-run-data)
       (make-specific-operation :increase-evolution-phase))))
    :elite-manager
    (make-elite-manager :max-size 10)))
