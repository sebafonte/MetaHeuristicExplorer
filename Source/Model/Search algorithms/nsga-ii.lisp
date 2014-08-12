;;
;; NSGA-II: Non Dominated Sort Genetic Algorithm, a multi-objective optimization algorithm
;; 
;; References
;;  - http://sci2s.ugr.es/docencia/doctobio/2002-6-2-DEB-NSGA-II.pdf
;;  - http://www.iitk.ac.in/kangal/codes.shtml
;;  - http://www.cleveralgorithms.com/nature-inspired/evolution/nsga.html#Goldberg1989
;; 
;; #TODO: Check if it's a good idea to create a class for fronts (to hold crowding distance for example)
;;

;(defpackage "NSGA-2" (:use "CL")
;  (:export
;   "NSGA-2")
;   (:import-from "CL-USER"))

;(in-package "NSGA-2")


#|
TEST: 

A: 1, 3, 1
B: 8, 8, 0
C: 2, 3, 1
D: 2, 4, 1
|#

;; #OPTIMIZATION
(declaim (inline objective-value))

(defclass nsga-ii (search-algorithm)
  ((population :initarg :initial-population :accessor population)
   (population-size :initarg :population-size :accessor population-size)
   (selection-method :initarg :selection-method :accessor selection-method)
   (initialization-method :initarg :initialization-method :accessor initialization-method)
   (local-optimization :initarg :local-optimization :accessor local-optimization)
   (registry :initform (make-hash-table :test #'eql) :accessor registry)
   (max-generations :initarg :max-generations :initform 100 :accessor max-generations)
   ;; #TODO: Check if move this to fitness evaluator
   (objectives :initarg :objectives :accessor objectives)
   (generation :initform 0 :accessor generation)
   (best :initarg :best :initform nil :accessor best)
   ;; fast-non-dominated-sort cache
   (fronts :initarg :fronts :initform nil :accessor fronts)
   (dominance :initarg :dominance :accessor dominance)
   (dominated-count :initarg :dominated-count :accessor dominated-count)
   (rank :initarg :rank :accessor rank)
   (distance :initarg :distance :accessor distance)
   (individuals :initarg :individuals :accessor individuals)))


(defmethod initialize-properties :after ((a nsga-ii))
  "Initialize <a> properties."
  (let ((objectives (default-objectives 'entity-function-x)))
    (add-properties-from-values
     a
     (:name 'name :label "Name" :accessor-type 'accessor-accessor-type :data-type 'symbol
      :default-value "NSGA-II" :editor 'text-editor)
     (:name 'max-generations :label "Max generations" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :min-value 0 :max-value 100000 :default-value 200 :editor 'number-editor
      :object-parameter t)
     (:name 'generation :label "Generation" :accessor-type 'valuable-accessor-type 
      :data-type 'integer :default-value 0 :read-only t :editor 'number-editor :getter #'generation)
     (:name 'population :label "Population" :accessor-type 'accessor-accessor-type 
      :data-type 'population :default-value nil :editor 'button-editor)
     (:name 'population-size :label "Population size" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :min-value 0 :max-value 1000000 :default-value 100 :editor 'lisp-editor)
     (:name 'selection-method :label "Selection method" :accessor-type 'accessor-accessor-type
      :data-type 'symbol :default-value (system-get 'tournament-selection-method)
      :possible-values (system-selection-methods) :editor 'configurable-copy-list-editor)
     (:name 'objectives :label "Objectives" :accessor-type 'accessor-accessor-type :setter '(setf objectives)
      :data-type 'list-structure :default-value objectives :possible-values (list objectives) :editor 'object-list-probability-editor)
     (:name 'initialization-method :label "Initialization" :accessor-type 'accessor-accessor-type 
      :data-type 'symbol :default-value (system-get 'random-trees-initializer) :editor 'configurable-copy-list-editor
      :dependency (make-possible-class-dependency 'objective-class)
      :default-value-function (lambda (objective-class) (copy-cyclic (first (possible-initialization-methods (make-instance objective-class)))))
      :possible-values-function (lambda (objective-class) (possible-initialization-methods (make-instance objective-class))))
     (:name 'local-optimization :label "Local optimization" :accessor-type 'accessor-accessor-type 
      :data-type 'symbol :default-value nil :possible-values (optimization-strategies) 
      :editor 'configurable-copy-list-editor)
     (:name 'best :label "Best individual" :accessor-type 'accessor-accessor-type :read-only t
      :data-type 'object :default-value nil :editor 'button-editor)
     (:name 'pareto-front :label "Pareto front" :accessor-type 'valuable-accessor-type :read-only t
      :data-type 'object :editor 'button-editor :getter 'pareto-front))))

(defmethod (setf objectives) (objectives (a nsga-ii))
  (setf (slot-value a 'objectives) (normalize-operation-list objectives)))

(defmethod pareto-front ((a nsga-ii))
  (first (fronts a)))

(defmethod copy ((o nsga-ii))
  (let ((copy (copy-instance o))
        (new-objectives (funcall (ttrav #'cons (lambda (x) (copy x))) (objectives o))))
    (setf (objectives copy) new-objectives)
    copy))

(defmethod select-genetic-operation ((a nsga-ii))
  "Answer a genetic operation for <a>."
  (get-random-element (operators a)))

(defmethod copy ((o nsga-ii))
  (when (local-optimization o)
    (setf (subject (local-optimization o)) nil))
  (let ((new-instance (copy-instance o)))
    (when (local-optimization o)
      (setf (subject (local-optimization new-instance)) new-instance
            (subject (local-optimization o)) o))
    new-instance))

(defmethod default-progress-indicator-value ((a nsga-ii))
  (generation a))

(defmethod all-individuals ((a nsga-ii))
  (individuals (population a)))

(defmethod register-best-individual ((a nsga-ii) population)
  (setf (best a) (best-individual-on a population)))

(defmethod best-individual ((a nsga-ii))
  "Anwer the best individual found by <a>."
  (best-individual-on a (population a)))

(defmethod best-individual-on ((a nsga-ii) population)
  "Anwer the best individual found by <a>."
  (if population 
      (first (sort (individuals population)
                   (lambda (x y) 
                     (< 
                      (weighted-sum-objectives a x) 
                      (weighted-sum-objectives a y)))))))

(defmethod generate-initial-population ((a nsga-ii))
  "Generate <a> initial population."
  (setf (population a) (generate-population (initialization-method a) a))
  (normalize-population-fitness (population a) #'fitness)
  (calculate-objectives a (individuals (population a))))

(defmethod initialize-dominance-matrix ((a nsga-ii) (p population))
  (let ((size (size p)))
    (setf fronts nil
          (individuals a) (individuals-array p)
          (rank a) (make-array (list size) :element-type 'integer)
          (distance a) (make-array (list size) :element-type 'integer)
          (dominated-count a) (make-array (list size) :element-type 'integer)
          (dominance a) (make-array (list size size) :element-type 'boolean))
    (dotimes (i size)
      (dotimes (j size)
        (setf (aref (rank a) i) -1
              (aref (dominance a) i j) nil))
      (setf (aref (dominated-count a) i) 0))))

(defmethod search-loop ((a nsga-ii) seed)
  ;; Generate initial population, front and register best individual
  (generate-initial-population a)
  (let ((p (population a)))
    (fast-nondominated-sort a p)
    (register-best-individual a p)
    ;; Create children using #'create-child which uses #'better-rank
    (let* ((selected (select-from a (loop for i from 0 to (1- (population-size a)) collect i) (population-size a)))
           (children (reproduce-child a selected (population-size a))))
      ;; #TODO: This could be avoided, this is done in the next step?
      (register-best-individual a p)
      (block nil
        (dotimes (i (max-generations a))
          (when (test-termination-nsga a)
            (return-from nil))
          (setf (generation a) i)
          (let* ((parents (select-pareto-parents a p children))
                 (selected (select-from a parents (population-size a))))
            (setf (individuals-array p) (to-array children)
                  children (reproduce-child a selected (population-size a)))
            ;; #NOTE: Point for possible local optimization
            (register-best-individual a p))
          (trigger a :progress-change :generation i))))))

(defmethod test-termination-best-individual ((a nsga-ii))
  "Answer whether the steady state search has to finish."
  (>= (fitness (best-individual a)) 
      (solution-fitness (fitness-evaluator a))))

(defmethod test-termination-nsga ((a nsga-ii))
  "Answer whether the steady state search has to finish."
  (let ((best-individual (best-individual a))
        (solution-fitness (solution-fitness (fitness-evaluator a))))
    (or (and solution-fitness
             (or (better-than-fitness-value best-individual solution-fitness)
                 (= (fitness best-individual) solution-fitness)))
        (and (max-evaluations a)
             (>= (evaluations (fitness-evaluator a))
                 (max-evaluations a))))))

(defmethod select-pareto-parents (algorithm population children)
  (let* ((union (to-array (append (individuals population) children)))
         ;; #TODO: Avoid population instanciation
         (population (make-population-with union)))
    (fast-nondominated-sort algorithm population)
    (trigger algorithm :pareto-first-front (first (fronts algorithm)))
    (select-fronts-parents algorithm population (population-size algorithm))))

(defmethod select-fronts-parents ((a nsga-ii) population count)
  (let ((fronts (fronts a))
        (offspring)
        (last-front 0))
    ;; Add all individuals of each front until no more fronts can be added
    (block nil
      (dolist (i fronts)
        (calculate-crowding-distance a population i)
        (when (> (+ (length offspring) (length i)) count)
          (return-from nil))
        (dolist (j i) (appendf offspring (list j)))
        (incf last-front)))
    ;; If we have to continue adding some individuals
    (let ((remaining (- count (length offspring))))
      (calculate-crowding-distance a population (nth last-front fronts))
      (when (> remaining 0)
        ;; Add individuals from last front until count is reached
        (let ((sorted-front (sort (nth last-front fronts) (lambda (x y) (crowded-comparison-operator a x y)))))
          (appendf offspring (subseq sorted-front 0 remaining)))))
    offspring))

;; #TODO: Move to selection function function when tested / (or fitness function?)
(defmethod select-from ((a nsga-ii) parents size)
  (loop for i from 0 to (1- size)
        collect 
        (better-rank a (random-element parents) (random-element parents))))

(defmethod reproduce-child ((a nsga-ii) parents count)
  (let* ((parents (to-array (mapcar (lambda (o) (aref (individuals a) o)) parents)))
         (children (create-individuals a count parents)))
    ;; #TODO: Check if move this to #'evaluate (nsga-ii)
    (calculate-objectives a children)
    children))

(defmethod fast-nondominated-sort ((a nsga-ii) (p population))
  "Answer a list with dominating fronts for <p>."
  (trigger a :nsga-sort-start)
  (initialize-dominance-matrix a p)
  (let* ((fronts (list nil))
         (array (individuals-array p))
         (size (size p))
         (changed t))
    ;; Build first front
    (dotimes (i size)
      (dotimes (j size)
        (let ((x (aref array i))
              (y (aref array j)))
          (if (pareto-dominates a x y)
              (setf (aref (dominance a) i j) t)
            (if (pareto-dominates a y x)
                (incf (aref (dominated-count a) i))))))S
        ;; If noone dominates i add it to first front and rank with 0
        (when (zerop (aref (dominated-count a) i))
          (setf (aref (rank a) i) 0)
          (appendf (first fronts) (list i))))
    ;; Create next fronts
    (do ((current 0))
        ((not changed))
      (setf changed nil)
      (let ((next-front))
        (dolist (i (nth current fronts))
          (dotimes (j size)
            (when (aref (dominance a) i j)
              (decf (aref (dominated-count a) j))
              (when (zerop (aref (dominated-count a) j))
                (setf (aref (rank a) j) (1+ current))
                (appendf next-front (list j))))))
        (incf current)
        (when next-front
          (appendf fronts (list next-front))
          (setf changed t))))
    (trigger a :nsga-sort-end)
    ;; Return fronts list
    (setf (fronts a) fronts)))

(defmethod create-individuals ((a nsga-ii) count individuals &optional (evaluate t))
  "Answer a new individual for <a>."
  (let ((result))
    (dotimes (i count)
      (let* ((operation (select-genetic-operation a))
             (child (make-instance (objective-class a)))
             ;; #TODO: Avoid population instanciation (profile)
             (population (make-population-with individuals))
             (parents (perform-selection (selection-method a) population (arity operation))))
        (create-child child a operation parents)
        (when evaluate (evaluate a child))
        (appendf result (list child))))
    result))

;; #TODO: Move this to aptitude evaluation and possibly OpenCL
(defmethod active-objectives ((a nsga-ii))
  (mapcar (lambda (o) (first o))
          (select (objectives a)
                  (lambda (o) (> (second o) 0.0)))))

(defmethod active-objectives-with-weight ((a nsga-ii))
  (select (objectives a)
          (lambda (o) (> (second o) 0.0))))

(defmethod calculate-objectives ((a nsga-ii) children)
  (dolist (i children)
    (dolist (o (active-objectives a))
      (setf (slot-value i (name o))
            (value-for-objective o i)))))

(defmethod weighted-sum-objectives (algorithm object)
  (reduce 'sum-with-inf (objective-values algorithm object)))

(defun sum-with-inf (a b)
  (if (or (eql a +1D++0) (eql b +1D++0))
      +1D++0
    (+ a b)))

(defmethod objective-value ((a nsga-ii) (o entity) (objective t))
  (declare (ignore a))
  (value-for-objective objective o))

#|
(defmethod objective-value ((a nsga-ii) (o entity) (objective integer))
  (declare (ignore a))
  (aref (objective-values o) objective))
|#

(defmethod objective-values ((a nsga-ii) (o entity))
  (map 'vector 
       (lambda (objective) 
         (let ((weight (second objective))
               (objective (first objective)))
           (* weight (value-for-objective objective o))))
       (active-objectives-with-weight a)))

(defun better-rank (a x y)
  (if (and 
       (not (null (aref (distance a) x)))
       (= (aref (rank a) x) (aref (rank a) y)))
      (if (> (aref (distance a) x) (aref (distance a) y))
          x 
        y)
    (if (< (aref (rank a) x) (aref (rank a) y))
        x 
      y)))

(defmethod pareto-dominates ((algorithm nsga-ii) a b)
  "Answer whether <a> pareto dominates <b> for <algorithm>, it means, not worse in every objective and at least in one better."
  (if (eql a b)
      nil
    (let ((dominated)
          (dominates))
      (dolist (o (active-objectives algorithm))
        (when (better-in a b o)
          (setf dominates t))
        (when (better-in b a o)
          (setf dominated t)))
      (and (not dominated) dominates))))

(defmethod better-in (x y objective)
  (<
   (value-for-objective objective x)
   (value-for-objective objective y)))

;; #TODO: Optimize and avoid #'to-array on 'individuals (profile)
(defmethod calculate-crowding-distance ((a nsga-ii) population individuals)
  (let* ((objectives (active-objectives a))
         (objectives-count (length objectives))
         (last-index (1- (length individuals)))
         (array (individuals-array population)))
    (dolist (i individuals)
      (setf (aref (distance a) i) 0))
    (dolist (i objectives)
      (let* ((sorted (sort individuals (lambda (x y) (objective-comparer i array x y))))
             (min (first sorted))
             (max (car (last sorted)))
             (rge (- (objective-value a (aref array max) i) (objective-value a (aref array min) i)))
             (sorted (to-array sorted)))
        (setf (aref (distance a) (aref sorted 0)) +1D++0
              (aref (distance a) (aref sorted last-index)) +1D++0)
        (when (not (= rge 0.0))
          (dotimes (j last-index)
            (when (not (= j 0))
              (incf (aref (distance a) (aref sorted j))
                    (/ (- (objective-value a (aref array (aref sorted (1+ j))) i)
                          (objective-value a (aref array (aref sorted (1- j))) i))
                       rge)))))))))

(defun objective-comparer (objective array x y)
  (< 
   (value-for-objective objective (aref array x))
   (value-for-objective objective (aref array y))))

(defun crowded-comparison-operator (algorithm x y)
  "Compares between ranks first, then distance <x> and <y>."
  (let ((rank-x (aref (rank algorithm) x))
        (rank-y (aref (rank algorithm) y)))
    (if (= rank-x rank-y)
        (< (aref (distance algorithm) x)
           (aref (distance algorithm) y))
      (< rank-x rank-y))))

(defclass moea-objective (object-with-properties)
  ((name :initarg :name :accessor name)
   (operator :initarg :operator :accessor operator)
   (weight :initarg :weight :accessor weight)
   (value-expression :initarg :value-expression :initform nil :accessor value-expression)))


(defmethod value-for-objective ((o moea-objective) object)
  (funcall (or (value-expression o) (name o)) object))

;; Default objectives for multi objective evolutionary algorithm on common f(x) functions, #REFACTOR
(defmethod default-objectives ((object (eql 'entity-function-x)))
  (list (list (subject (system-get 'moea-objective-distance))               0.5)
        (list (subject (system-get 'moea-objective-size))                   0.5)
        (list (subject (system-get 'moea-objective-non-linear-components))  0.0)))

(defun linear-components-count (exp)
  (recursive-count-list '(+ -) exp))

(defun non-linear-components-count (exp)
  (recursive-count-list '(* / /- exp sqrt sqr) exp))

(defun recursive-count-list (list exp)
  (reduce '+ (mapcar (lambda (o) (recursive-count o exp)) list)))

(defun recursive-count (item exp)
 (if (consp exp)
     (+ (recursive-count item (car exp))
        (recursive-count item (cdr exp)))
   (if (eql item exp)
       1
     0)))

(defun calculated-distance (fitness)
  (if (= fitness 0.0)
      +1D++0
      (- (/ 10 fitness) 1.0)))

(defclass entity (base-model)
  ((gen :initarg :gen :initform nil :accessor gen)
   (fitness :initarg :fitness :initform 0 :accessor fitness)
   (fitness-adjusted :initarg :fitness-adjusted :initform :fitness-adjusted :accessor fitness-adjusted)
   (fitness-normalized :initarg :fitness-normalized :initform :fitness-normalized :accessor fitness-normalized)
   ;; Patch instance variables by the moment #1 (objective values cache)
   (distance :initarg :distance :initform :inf :accessor distance)
   (size :initarg :size :initform 0 :accessor size)
   (non-linear-components :initarg :non-linear-components :initform 0 :accessor non-linear-components)))


(defmethod initialize-moea-objectives ()
  (system-add
   (make-instance 'registrable-object-wrapper 
                  :name 'moea-objective-distance
                  :description "Distance"
                  :subject (make-instance 'moea-objective 
                  :name 'distance 
                  :value-expression 'lambda-calculated-distance))
   (make-instance 'registrable-object-wrapper 
                  :name 'moea-objective-size
                  :description "Size"
                  :subject (make-instance 'moea-objective 
                  :name 'size 
                  :value-expression 'lambda-tree-size))
   (make-instance 'registrable-object-wrapper 
                  :name 'moea-objective-non-linear-components
                  :description "Non linear components"
                  :subject (make-instance 'moea-objective 
                  :name 'non-linear-components 
                  :value-expression 'lambda-non-linear-components-count))))

(defun lambda-calculated-distance (o)
  (calculated-distance (fitness o)))

(defun lambda-tree-size (o)
  (tree-size (program o)))

(defun lambda-non-linear-components-count (o)
  (non-linear-components-count (program o)))
