
(defclass population-generator (object-with-properties)
  ((name :initarg :name :accessor name)))


(defmethod initialize-properties :after ((object population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'population-generator :editor 'symbol-editor)))

(defmethod print-object ((o population-generator) seq)
  (format seq "~A" (name o)))

(defmethod generate-population ((generator population-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (error "Subclass responsibility."))

(defmethod generate-individual ((generator population-generator) algorithm)
  "Generate individual for search on <algorithm>."
  (error "Subclass responsibility."))

(defmethod operate ((generator population-generator) algorithm expressions)
  "Operate on <generator> to obtain a new object for search on <algorithm>."
  (declare (ignore expressions))
  (generate-individual generator algorithm))

; -----------------------------------------------------------------------------

(defclass random-binary-generator (population-generator)
  ((repeat-control :initarg :repeat-control :accessor repeat-control)))


(defmethod initialize-properties :after ((object random-binary-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'repeat-control :label "Repeat control" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)))

(defmethod generate-population ((p random-binary-generator) (a search-algorithm))
  "Generate population for search on <algorithm>.
  #NOTE: Fill population with new random invididuals."
  (if (repeat-control p)
      (generate-population-incremental-size-attempts p a)
    (generate-population-no-control p a)))

(defmethod generate-population-incremental-size-attempts ((p random-binary-generator) (a search-algorithm))
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (max-attempts (max-unique-iterations a))
         (attempts 0))
    (do ((i 0))
        ((>= i population-size))
      (let ((value (generate-individual-value p (language a))))
        (cond ;; 1 - New and not into registry
              ((not (gethash value (registry a)))
               (setf (aref population i) (make-instance (objetive-class a) :expresion value))
               (evaluate a (aref population i))
               (setf (gethash value (registry a)) t
                     attempts 0)
               (incf i))
              ;; 2 - New and into registry: max retries reached
              ((> attempts max-attempts))
              ;; 3 - Found, did not reach max attempts #LOG
              (t (incf attempts)))))
    ;; #TODO: this should be moved to algorithm calling function
    (clrhash (registry a))
    (make-instance 'population :count-individuals population-size :individuals-array population)))

(defmethod generate-population-no-control ((p random-binary-generator) (a search-algorithm))
  "Generate population for search on <algorithm>.
  #NOTE: Fill population with new random invididuals."
  (let* ((population-size (population-size a))
         (population (make-array population-size)))
    ;; Populate with random programs with no repeat control
    (do ((i 0))
        ((>= i population-size))
      (setf (aref population i) (generate-individual p a))
      (incf i))
    ;; Answer population object
    (let ((new-population (make-instance 'population :count-individuals population-size :individuals-array population)))
      (evaluate a new-population)
      new-population)))

(defmethod generate-individual-value ((generator random-binary-generator) language)
  "Answer a new generated program tree on <language> using <generator>."
  (create-new-random-valid language nil))

(defmethod generate-individual ((generator random-binary-generator) algorithm)
  "Answer a new generated object on <language> using <generator>."
  (make-instance (objetive-class algorithm) 
                 :expresion (generate-individual-value generator (language algorithm))))

; -----------------------------------------------------------------------------

(defclass random-trees-generator (population-generator)
  ((min-size :initarg :min-size :accessor min-size)
   (max-size :initarg :max-size :accessor max-size)
   (min-depth :initarg :min-depth :accessor min-depth)
   (max-depth :initarg :max-depth :accessor max-depth)
   (repeat-control :initarg :repeat-control :accessor repeat-control)))


(defmethod initialize-properties :after ((object random-trees-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'min-size :label "Min size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 1 :editor 'number-editor)
   (:name 'max-size :label "Max size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor)
   (:name 'min-depth :label "Min depth" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 1 :editor 'number-editor)
   (:name 'max-depth :label "Max depth" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor)
   (:name 'repeat-control :label "Repeat control" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)))

(defmethod generate-population ((p random-trees-generator) (a search-algorithm))
  "Generate population for search on <algorithm>.
  #NOTE: Fill population with new random invididuals."
  (if (repeat-control p)
      (generate-population-incremental-size-attempts p a)
    (generate-population-no-control p a)))

(defmethod generate-population-incremental-size-attempts ((p random-trees-generator) (a search-algorithm))
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (min-tree-depth 0)
         (max-attempts (max-unique-iterations a))
         (attempts 0))
    (do ((i 0))
        ((>= i population-size))
      (let ((program (generate-program-tree p (language a))))
        (cond ;; 1 - New and not into registry
              ((not (gethash program (registry a)))
               (setf (aref population i) (make-instance (objetive-class a) :expresion program))
               (evaluate a (aref population i))
               (setf (gethash program (registry a)) t
                     attempts 0)
               (incf i))
              ;; 2 - New and into registry: max retries reached
              ((> attempts max-attempts)
               (incf min-tree-depth))
              ;; 3 - Found, did not reach max attempts #LOG
              (t (incf attempts)))))
    ;; #TODO: this should be moved to algorith calling function
    (clrhash (registry a))
    (make-instance 'population :count-individuals population-size :individuals-array population)))

(defmethod generate-population-no-control ((p random-trees-generator) (a search-algorithm))
  "Generate population for search on <algorithm>.
  #NOTE: Fill population with new random invididuals."
  (let* ((population-size (population-size a))
         (population (make-array population-size)))
    ;; Populate with random programs with no repeat control
    (do ((i 0))
        ((>= i population-size))
      (let ((program (generate-program-tree p (language a))))
        (setf (aref population i) (make-instance (objetive-class a) :expresion program))
        (incf i)))
    ;; Answer population object
    (let ((new-population (make-instance 'population :count-individuals population-size :individuals-array population)))
      (evaluate a new-population)
      new-population)))

(defmethod generate-program-tree ((generator random-trees-generator) language)
  "Answer a new generated program tree on <language> using <generator>."
  (let ((max-size (max-size-new-individuals language))
        (max-depth (max-depth-new-individuals language)))
    (simplify language (create-expresion language max-size max-depth t t)))) 

(defmethod generate-individual ((generator random-trees-generator) algorithm)
  "Answer a new generated object on <language> using <generator>."
  (make-instance (objetive-class algorithm) 
                 :expresion (generate-program-tree generator (language algorithm))))

; -----------------------------------------------------------------------------

(defclass random-trees-generator-with-operator (random-trees-generator)
  ((operator :initarg :operator :accessor operator)))


(defmethod initialize-properties :after ((object random-trees-generator-with-operator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'operator :label "Operator" :editor 'button-editor
    :data-type 'object :accessor-type 'accessor-accessor-type)))

(defmethod generate-program-tree ((generator random-trees-generator-with-operator) language)
  "Answer a new generated program tree on <language> using <generator>."
  (simplify language (operate (operator generator) language nil)))

(defun random-create-cfg-initial-size (program language operator)
  (declare (ignore program))
  (let ((weight-function (production-selection-weight-function operator)))
    (create-random-from-production language '(start) (max-size-new-individuals language) weight-function)))

; -----------------------------------------------------------------------------

(defclass ramped-half-and-half-generator (population-generator)
  ((min-value :initarg :min-value :accessor min-value)
   (max-value :initarg :max-value :accessor max-value)
   (distribution :initarg :distribution :accessor distribution)))


(defmethod initialize-properties :after ((object ramped-half-and-half-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'ramped-half-and-half-generator :editor 'symbol-editor)
   (:name 'min-value :label "Min size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 1 :editor 'number-editor)
   (:name 'max-value :label "Max size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor)
   (:name 'distribution :label "Distribution" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value '(lambda (value) 1) :editor 'lisp-editor)))
   
(defmethod generate-population ((p ramped-half-and-half-generator) (a search-algorithm))
  "Generate population for search on <algorithm>."
  (error "Not implemented yet."))

; -----------------------------------------------------------------------------

(defclass fixed-solutions-generator (population-generator)
  ((expresion-list :initarg :expresion-list :accessor expresion-list)))


(defmethod initialize-properties :after ((object fixed-solutions-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'fixed-solutions-generator :editor 'symbol-editor)
   (:name 'expresion-list :label "Expression list" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :editor 'one-line-lisp-editor)))

(defmethod generate-population ((generator fixed-solutions-generator) 
                                (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size))
         (fixed-solutions (expresion-list (initialization-method algorithm)))
         (size (length fixed-solutions)))
    (dotimes (i population-size)
      (let ((object (make-instance (objetive-class algorithm) :expresion (nth (mod i size) fixed-solutions))))
        (setf (aref population i) object)))
    (evaluate algorithm population)
    (make-instance 'population :count-individuals population-size :individuals-array population)))

; -----------------------------------------------------------------------------

(defclass sample-linear-ordering-population-generator (population-generator)
  ((initial-matrix :initarg :initial-matrix :accessor initial-matrix)
   (initial-matrix-file :initarg :initial-matrix-file :accessor initial-matrix-file)
   (initial-permutations :initarg :initial-permutations :accessor initial-permutations)))


(defmethod initialize-properties :after ((object sample-linear-ordering-population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'initial-matrix :label "Initial matrix" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value nil :editor 'lisp-editor)
   (:name 'initial-matrix-file :label "Initial matrix file" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'file-prompter-editor :setter '(setf initial-matrix-file))
   (:name 'initial-permutations :label "Initial permutations" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 10 :editor 'integer-editor)))

(defmethod (setf initial-matrix-file) (file (o sample-linear-ordering-population-generator))
  "Sets the initial file string in initial-matrix-file slot of <o>."
  (setf (slot-value o 'initial-matrix-file) file)
  (load-initial-matrix-data o))

(defmethod load-initial-matrix-data ((o sample-linear-ordering-population-generator))
  "Loads the initial matrix data of <o>."
  (when (initial-matrix-file o)
    (setf (initial-matrix o) 
          (lop-matrix-from (load-lop-matrix-description (initial-matrix-file o))))))

(defmethod lop-matrix-from (description)
  (let* ((size (first description))
         (rest (cdr description))
         (array (make-array (list size size))))
    (dotimes (i size)
      (dotimes (j size)
        (setf (aref array i j) (car rest)
              rest (cdr rest))))
    array))

(defmethod generate-population ((generator sample-linear-ordering-population-generator) 
                                (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size))
         (initial-matrix (initial-matrix generator)))
    (dotimes (i population-size)
      (let ((object (make-instance (objetive-class algorithm) 
                                   :matrix initial-matrix :initial-matrix initial-matrix)))
        (dotimes (i (initial-permutations generator))
          (setf object (permutate-random-row object algorithm nil)
                object (permutate-random-column object algorithm nil)))
        (evaluate algorithm object)
        (setf (aref population i) object)))
    (make-instance 'population :count-individuals population-size :individuals-array population)))

; -----------------------------------------------------------------------------

(defclass sample-vrp-population-generator (population-generator)
  ((generation-method :initarg :generation-method :accessor generation-method)))


(defmethod initialize-properties :after ((object sample-vrp-population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'generation-method :label "Generation method" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'random-tour-variable-vehicles :editor 'list-editor
    :possible-values (possible-generation-methods object))))

(defmethod possible-generation-methods ((object sample-vrp-population-generator))
  '(radial-scan-generation-method
    one-vehicle-per-city-method
    random-tour-fixed-vehicles
    random-tour-variable-vehicles
    clark-and-wright-generation))

(defmethod generate-population ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (funcall (generation-method object) object algorithm))

(defmethod clark-and-wright-generation ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generation method which fills the entire population of the cw solution or it mutations."
  (let* ((cw-solution (clark-and-wright-parallel (fitness-evaluator algorithm)))
         (population-size (population-size algorithm))
         (individuals (make-array population-size)))
    (dotimes (i (population-size algorithm))
      (let ((new-object (make-instance 'entity-sample-vrp :expresion cw-solution)))
        (evaluate algorithm new-object)
        (setf (aref individuals i) new-object)))
    (make-instance 'population :count-individuals population-size :individuals-array individuals)))

(defmethod one-vehicle-per-city-method ((object sample-vrp-population-generator) (algorithm search-algorithm))
 "Generation method which implements radial scanning heuristic."
 (let* ((population-size (population-size algorithm))
        (population (make-array population-size)))
   (dotimes (i population-size)
     (let* ((plan (list (list 0 1)))
            (individual (make-instance (objetive-class algorithm) :expresion plan)))
       (evaluate algorithm individual)
       (setf (aref population i) individual)))
   (make-instance 'population :count-individuals population-size :individuals-array population)))

(defmethod random-tour-variable-vehicles ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generation method which generates random tours for a variable amount of vehicles."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size))
         (min-tour-size 1)
         (max-tour-size 10)
         (delta (- max-tour-size min-tour-size))
         (cities-count (1- (cities-count (fitness-evaluator algorithm)))))
    (dotimes (i (population-size algorithm))
      (let ((tour-size (+ min-tour-size (mod i delta)))
            (city-list (loop for i from 1 to cities-count collect i))
            (hash-table (make-hash-table)))
        (dotimes (j cities-count)
          (let ((tour-index (1+ (mod j tour-size)))
                (next-city (select-random-city object city-list)))
            (removef city-list next-city)
            (appendf (gethash tour-index hash-table) (list next-city))))
        (let ((individual (construct-individual-from object hash-table)))
          (evaluate algorithm individual)
          (setf (aref population i) individual))))
    (make-instance 'population :count-individuals population-size :individuals-array population)))

(defmethod select-random-city ((object sample-vrp-population-generator) list)
  (nth (random-integer 0 (length list)) list))

(defmethod construct-individual-from ((object sample-vrp-population-generator) hash-table)
  (let ((tour-list))
    (dolist (key (keys hash-table))
      (appendf tour-list (list (gethash key hash-table))))
    (make-instance 'entity-sample-vrp :expresion tour-list)))

(defmethod radial-scan-generation-method ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generation method which implements radial scanning heuristic."
  nil)

(defmethod random-tour-fixed-vehicles ((object sample-vrp-population-generator) (algorithm search-algorithm))
  "Generation method which generates random tours for a fixed amount of vehicles."
  nil)

; -----------------------------------------------------------------------------

(defclass property-sampling-population-generator (population-generator)
  ((properties-sampling-description 
    :initarg :properties-sampling-description :accessor properties-sampling-description)))


(defmethod initialize-properties :after ((object property-sampling-population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'properties-sampling-description :label "Properties sampling description" 
    :accessor-type 'accessor-accessor-type :data-type 'list-structure :default-value nil :editor 'lisp-editor)))

;;; #TODO:
(defmethod generate-population ((object property-sampling-population-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  nil)

; -----------------------------------------------------------------------------

(defclass polynomial-sample-generator (population-generator)
  ((min-terms :initarg :min-terms :accessor min-terms)
   (max-terms :initarg :max-terms :accessor max-terms)
   (min-factor :initarg :min-factor :accessor min-factor)
   (max-factor :initarg :max-factor :accessor max-factor)
   (min-grade :initarg :min-grade :accessor min-grade)
   (max-grade :initarg :max-grade :accessor max-grade)))


(defmethod initialize-properties :after ((object polynomial-sample-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'min-terms :label "Min terms" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 1 :editor 'integer-editor)
   (:name 'max-terms :label "Max terms" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 3 :editor 'integer-editor)
   (:name 'min-factor :label "Min factor" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 0 :editor 'number-editor)
   (:name 'max-factor :label "Max factor" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 10 :editor 'number-editor)
   (:name 'min-grade :label "Min grade" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 1 :editor 'integer-editor)
   (:name 'max-grade :label "Max grade" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 4 :editor 'integer-editor)))

(defmethod generate-population ((object polynomial-sample-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (let* ((variables (variables (language algorithm)))
         (population-size (population-size algorithm))
         (population (make-array population-size)))
    (dotimes (i (population-size algorithm))
      (let* ((terms (random-integer (min-terms object) (max-terms object)))
             (new-exp '(+)))
        (dotimes (i terms)
          (let ((factor (random-integer (min-factor object) (max-factor object)))
                (grade (random-integer (min-grade object) (max-grade object)))
                (variable-list))
            (dotimes (j grade)
              (appendf variable-list (list (random-element variables))))
            (appendf new-exp (list (list '* factor (cons '* variable-list))))))
        (let ((object (make-instance (objetive-class algorithm) :expresion new-exp)))
          (setf (aref population i) object))))
    (let ((new-population (make-instance 'population :count-individuals population-size :individuals-array population)))
      (evaluate algorithm new-population)
      new-population)))
    
; -----------------------------------------------------------------------------

(defclass search-task-sample-generator (population-generator)
  ((sample-objects :initarg :sample-objects :accessor sample-objects)
   (mutate-individuals :initarg :mutate :accessor mutate-individuals)))


(defmethod initialize-properties :after ((object search-task-sample-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'sample-objects :label "Sample tasks" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value (default-objects object) :editor 'lisp-editor)
   (:name 'mutate-individuals :label "Mutate" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)))

(defmethod generate-population ((object search-task-sample-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size)))
    (dotimes (i (population-size algorithm))
      (let ((new-exp (random-element (sample-objects object))))
        (if (mutate-individuals object)
          (setf new-exp (operate mutation-operator object algorithm)))
        (let ((object (make-instance (objetive-class algorithm) :expresion new-exp)))
          (evaluate algorithm object)
          (setf (aref population i) object))))
    (make-instance 'population :count-individuals population-size :individuals-array population)))

(defmethod default-objects ((object search-task-sample-generator))
  (list
   ;; Simple task
   '(BEST-OF-TASK
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG-OBJ 40 0 10)
      (MAKE-GN-RND-OBJ 1)
      (MAKE-FE-OBJ 1)))
   ;; Composite task (paralell)
   '(BEST-OF-TASKS
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG-OBJ 40 0 10)
      (MAKE-GN-RND-OBJ 1)
      (MAKE-FE-OBJ 1))
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG-OBJ 40 0 10)
      (MAKE-GN-RND-OBJ 1)
      (MAKE-FE-OBJ 1)))
   ;; Composite task (combined results, 2 levels)
   '(BEST-OF-TASKS
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG-OBJ 40 0 10)
      (MAKE-GN-BESTS-OBJ (MAKE-TASK
                          (MAKE-BUILDER-IT 10)
                          (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
                          (MAKE-LG-OBJ 40 0 10)
                          (MAKE-GN-RND-OBJ 1)
                          (MAKE-FE-OBJ 1)))
      (MAKE-FE-OBJ 1)))
   ;; Composite task (combined results, 3 levels)
   '(BEST-OF-TASKS
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG-OBJ 40 0 10)
      (MAKE-GN-BESTS-OBJ (MAKE-TASK
                          (MAKE-BUILDER-IT 5)
                          (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
                          (MAKE-LG-OBJ 40 0 10)
                          (MAKE-GN-BESTS-OBJ (MAKE-TASK
                                              (MAKE-BUILDER-IT 5)
                                              (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
                                              (MAKE-LG-OBJ 40 0 10)
                                              (MAKE-GN-RND-OBJ 1)
                                              (MAKE-FE-OBJ 1)))
                          (MAKE-FE-OBJ 1)))
      (MAKE-FE-OBJ 1)))))

; -----------------------------------------------------------------------------

(defclass task-best-objects-initializer (population-generator)
  ((input-task :initarg :input-task :accessor input-task)
   (random-select :initarg :random-select :accessor random-select)
   (mutate-individuals :initarg :mutate-individuals :accessor mutate-individuals)))


(defmethod initialize-properties :after ((object task-best-objects-initializer))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'input-task :label "Input task" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'random-select :label "Random select" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value t :editor 'boolean-editor)
   (:name 'mutate-individuals :label "Mutate" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)))

(defmethod generate-population ((object task-best-objects-initializer) (algorithm search-algorithm))
  "Generate population on <algorithm>."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size))
         (individuals (best-individuals (input-task object) population-size)))
    (dotimes (i (population-size algorithm))
      (setf (aref population i) (random-element individuals)))
    (make-instance 'population :count-individuals population-size :individuals-array population)))

; -----------------------------------------------------------------------------

(defclass combined-population-generator (population-generator)
  ((operations :initarg :operations :accessor operations)))


(defmethod initialize-properties :after ((object combined-population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'combined-population-generator :editor 'symbol-editor)
   (:name 'operations :label "Operations" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :editor 'list-editor)))

(defmethod generate-population ((p combined-population-generator) (a search-algorithm))
  "Generate <p> for <a>."
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (max-attempts (max-unique-iterations a))
         (attempts 0))		 
    (do ((i 0))
        ((>= i population-size))
      (let ((child (create-with-random-operation p a)))
        (cond ;; 1- New and not in registry
              (t 
               (evaluate a child)
               (setf (gethash (program child) (registry a)) t
                     attempts 0
                     (aref population i) child) 
               (incf i))
              ;; 2 - New and found intro registry and max attempts have reached
              ((> attempts max-attempts)
               (incf min-tree-depth))
              ;; 3 - Max attempts have'nt been reached
              (t (incf attempts)))))
    (clrhash (registry a))
    (make-instance 'population :count-individuals population-size :individuals-array population)))

(defmethod create-with-random-operation ((p combined-population-generator) (a search-algorithm))
  "Answer a new created object using <p> creation operators."
  (operate 
   (operation (first (random-elements (operations p) 1)))
   a
   nil))

; -----------------------------------------------------------------------------

(defclass evolutive-algorithm-sample-generator (search-task-sample-generator)
  ())


(defmethod initialize-properties :after ((o evolutive-algorithm-sample-generator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'sample-objects :label "Sample algorithms" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value (default-objects o) :editor 'lisp-editor)))

(defmethod default-objects ((o evolutive-algorithm-sample-generator))
  (list
   (sample-composite-algorithm-code-1)))

; -----------------------------------------------------------------------------

(defun initialize-default-population-generators ()
  (system-add
   (make-instance 'random-binary-generator :name 'random-binary-initializer)
   (make-instance 'random-trees-generator :name 'random-trees-initializer)
   (make-instance 'random-trees-generator-with-operator
                  :name 'random-trees-cfg-initializer
                  :operator (system-get-copy 'initial-random-create-cfg-1))
   (make-instance 'fixed-solutions-generator :name 'fixed-expressions-initializer)
   (make-instance 'ramped-half-and-half-generator :name 'ramped-half-and-half-initializer)
   (make-instance 'sample-linear-ordering-population-generator :name 'sample-lop-initializer)
   (make-instance 'sample-vrp-population-generator :name 'sample-vrp-initializer)
   (make-instance 'property-sampling-population-generator :name 'sample-property-sampling-initializer)
   (make-instance 'polynomial-sample-generator :name 'sample-random-polynomial-generator)
   (make-instance 'search-task-sample-generator :name 'sample-search-task-generator)
   (make-instance 'evolutive-algorithm-sample-generator :name 'evolutive-algorithm-sample-generator)
   (make-instance 'task-best-objects-initializer :name 'task-best-objects-initializer)))

(defun system-population-initializer-methods ()
  (list 
   (system-get 'random-binary-initializer)
   (system-get 'random-trees-initializer)
   (system-get 'random-trees-cfg-initializer)
   (system-get 'fixed-expressions-initializer)
   (system-get 'ramped-half-and-half-initializer)
   (system-get 'sample-lop-initializer)
   (system-get 'sample-random-polynomial-generator)
   (system-get 'sample-vrp-initializer)
   (system-get 'sample-search-task-generator)
   (system-get 'evolutive-algorithm-sample-generator)
   (system-get 'task-best-objects-initializer)))
