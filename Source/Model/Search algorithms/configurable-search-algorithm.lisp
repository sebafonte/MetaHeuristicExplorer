;;
;; 1- Define parser for creation - OK
;; 2- Define language for creation - OK
;; 3- Define search-algorithm, breeder, evolver - OK
;; 4- Check if creation method is enough - (COULD WORK)...
;; 5- Instanciate different default algorithms - OK
;; 5- Include this class as a possible search algorithm - OK
;; 6- Test some of these default algorithms - OK BASIC 1, WITHOUT ITERATOR
;; 7- Complete iterator functionalities
;; 8- Create some unit tests
;; 9- Implement some book classic examples
;; 10- Tune!
;;

(defclass configurable-search-algorithm (search-algorithm entity)
  ((population-size :initarg :population-size :accessor population-size)
   (registry :initform (make-hash-table :test #'equal) :accessor registry)
   (program :initarg :program :accessor program)
   (initializer :initarg :initializer :accessor initializer)
   (evolver :initarg :evolver :accessor evolver)
   (population :initarg :population :accessor population)
   (elite-manager :initarg :elite-manager :accessor elite-manager)
   ;; #TODO: Delete this instance var and set the task
   (language :initarg :language :accessor language)))


(defmethod initialize-properties :after ((a configurable-search-algorithm))
  "Initialize <a> properties."
  (add-properties-from-values
   a 
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'configurable-search-algorithm :editor 'text-editor)
   (:name 'program :label "Program" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :editor 'lisp-editor)
   (:name 'evolver :label "Evolver" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'population :label "Population" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'initializer :label "Initializer" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'elite-manager :label "Elite manager" :accessor-type 'accessor-accessor-type 
    :data-type 'object :default-value (make-instance 'elite-manager) :editor 'button-editor)
   ;; #TODO: Quitar overrides
   (:name 'iteration :label "Iteración" :default-value 0 :accessor-type 'accessor-accessor-type 
    :data-type 'integer :read-only t :editor 'number-editor :visible nil)
   (:name 'max-iterations :label "Max iterations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 10000 :editor 'number-editor
    :visible nil)
   ;; #NOTE: Interface auxiliars
   (:name 'best-individual :label "Best individual" :accessor-type 'valuable-accessor-type 
    :getter 'lambda-configurable-algorithm-best-individual
    :data-type 'object :min-value 0 :max-value 100000 :default-value 10000 :editor 'button-editor)))

(defun lambda-configurable-algorithm-best-individual (object)
  (make-instance 'object-in-search :object (best-individual object) :context object))

(defmethod (setf evolver) (evolver (a configurable-search-algorithm))
  (setf (slot-value a 'evolver) evolver
        (algorithm evolver) a))

(defmethod search-loop ((a configurable-search-algorithm) seed)
  "Search method implementation for <a>." 
  ;; #TODO: Refactor into a builder object or something
  (let ((evolver (evolver a)))
    (setf (algorithm evolver) a
          (evolver (iterator evolver)) evolver)
    (initialize-population a)
    (evolve evolver)))

(defmethod initialize-population ((a configurable-search-algorithm))
  "Initialize <a> population."
  (setf (population a)
        (generate-population (initializer a) a)))

(defmethod (setf population) (value (a configurable-search-algorithm))
  (when (not (eql (population a) value))
    (setf (slot-value a 'population) value)))

(defmethod best-individual ((a configurable-search-algorithm))
  "Anwer the best individual found by <a>."
  (if (population a)
      (best-individual (population a))))

;;; #TODO: Refactor, this is horrible
(defmethod evolvablep ((o configurable-search-algorithm))
  "Answer whether <o> can be evolved."
  t)

(defmethod default-progress-indicator-value ((a configurable-search-algorithm))
  (evolution-phase (evolver a)))

(defmethod register-elites ((e evolver))
  "Check elites and update <e>."
  (let* ((algorithm (algorithm e))
         (manager (elite-manager algorithm))
         (population (population algorithm)))
    (check-elite manager population)
    (update-population manager population)))

(defmethod default-fitness-evaluators ((object configurable-search-algorithm))
  "Answer the default classes that can evaluate <object> fitness."
  (list 
   (system-get 'default-composite-algorithm-evaluator-1)))

(defmethod register-run-data ((a configurable-search-algorithm))
  "Register run data into <a>."
  nil)

(defmethod possible-languages ((o configurable-search-algorithm))
  (list 
   (system-get 'evolutive-algorithm-language)))

(defmethod population ((a configurable-search-algorithm))
  "Answer <a> population."
  (population a))

(defclass operation-description (object-with-properties)
  ((operation :initarg :operation :accessor operation)
   (weight :initarg :weight :accessor weight)))

(defmethod initialize-properties :after ((o operation-description))
  "Initialize <o> properties."
  (add-properties-from-values
   o 
   (:name 'operation :label "Operation" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'weight :label "Weigth" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor)))

(defmethod execute-operation-on ((o operation-description) algorithm)
  (operate (operation o) algorithm (select-parents o algorithm)))


(defclass unary-operation-description (operation-description)
  ((selection-function :initarg :selection-function :accessor selection-function)
   (node-weight-function :initarg :node-weight-function :accessor node-weight-function)
   (child-selection :accessor child-selection)))

(defmethod initialize-properties :after ((o unary-operation-description))
  "Initialize <o> properties."
  (add-properties-from-values
   o 
   (:name 'selection-function :label "Selection" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'node-weight-function :label "Node weigth" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'button-editor)
   (:name 'child-selection :label "Child selection" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)))

(defmethod select-parents ((o unary-operation-description) algorithm)
  (mapcar (lambda (o) (program o))
          (perform-selection (selection-function o) (population algorithm) 1)))


(defclass binary-operation-description (operation-description)
  ((selection-a-function :initarg :selection-a-function :accessor selection-a-function)
   (selection-b-function :initarg :selection-b-function :accessor selection-b-function)
   (node-weight-function-a :initarg :node-weight-function-a :accessor node-weight-function-a)
   (node-weight-function-b :initarg :node-weight-function-b :accessor node-weight-function-b)
   (child-selection-a :initarg :child-selection-a :accessor child-selection-a)
   (child-selection-b :initarg :child-selection-b :accessor child-selection-b)))

(defmethod initialize-properties :after ((o binary-operation-description))
  "Initialize <o> properties."
  (add-properties-from-values
   o 
   (:name 'selection-a-function :label "Selection A" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'selection-b-function :label "Selection B" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'node-weight-function-a :label "Node weigth A" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'button-editor)
   (:name 'node-weight-function-b :label "Node weigth B" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'child-selection-a :label "Child selection B" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'child-selection-b :label "Child selection B" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)))

(defmethod select-parents ((o binary-operation-description) algorithm)
  (mapcar (lambda (o) (program o))
          (append (perform-selection (selection-a-function o) (population algorithm) 1)
                  (perform-selection (selection-b-function o) (population algorithm) 1))))


(defclass evolver (object-with-properties)
  ((iterator :initarg :iterator :accessor iterator)
   (algorithm :initarg :algorithm :accessor algorithm)
   (operations :initarg :operations :accessor operations)
   (child-iterations :initarg :child-iterations :accessor child-iterations)))

(defmethod initialize-properties :after ((e evolver))
  "Initialize <e> properties."
  (add-properties-from-values
   e 
   (:name 'iterator :label "Iterator" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'algorithm :label "Algorithm" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'operations :label "Operations" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'button-editor)
   (:name 'child-iterations :label "Child iterations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :editor 'integer-editor)))

(defmethod population ((e evolver))
  (population (algorithm e)))

(defmethod evolve ((e evolver))
  (until-do (termination-condition e)
            (iterate (iterator e))))

(defmethod termination-condition ((e evolver))
  (test-termination (algorithm e) e))

(defmethod add-operation-description ((e evolver) operation-description)
  (appendf (operations e) operation-description))

(defmethod remove-operation-description ((e evolver) operation-description)
  (removef (operations e) operation-description))

(defmethod select-operation ((e evolver))
  (random-element (operations e)))

(defmethod breed-individual-into ((e evolver) source-population target-population)
  (let* ((algorithm (algorithm e))
         (operation (select-operation e))
         (new-exp (execute-operation-on operation algorithm))
         (object (make-instance (objetive-class algorithm) :expresion new-exp)))
    (replace-individual-into e operation object target-population)))

(defmethod register-run-data ((e evolver))
  nil)

(defclass iterational-evolver (evolver)
  ((max-iterations :initarg :max-iterations :accessor max-iterations)
   (iteration :initarg :iteration :initform 0 :accessor iteration)))

(defmethod initialize-properties :after ((o iterational-evolver))
  "Initialize <o> properties."
  (add-properties-from-values
   o 
   (:name 'max-iterations :label "Max iterations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :editor 'integer-editor :default-value 100)
   (:name 'iteration :label "Iteration" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :editor 'integer-editor :default-value 0)))

(defmethod evolution-phase ((e iterational-evolver))
  (iteration e))

(defmethod test-termination ((a configurable-search-algorithm) (e iterational-evolver))
  (or (>= (iteration e) (max-iterations e))
      (and (max-evaluations a) 
           (> (evaluations (fitness-evaluator a))
              (max-evaluations a)))))

(defmethod increase-evolution-phase ((e iterational-evolver))
  "Increase <e> evolution phase counter."
  (incf (iteration e)))

(defmethod replace-individual-into ((e iterational-evolver) operation object population)
  "Replace <object> into <population> for <operation>."
  (when (not (includes-equals (individuals population) object))
    (evaluate (algorithm e) object)
    (let ((index (select-replacement-index e operation object)))
      (setf (aref (individuals-array population) index) object))))

(defmethod breed ((b iterational-evolver))
  (let ((population (population b)))
    (breed-individual-into b population population)))


(defclass generational-evolver (evolver)
  ((new-population :initarg :new-population :accessor new-population)
   (max-generations :initarg :max-generations :accessor max-generations)
   (generation :initarg :generation :accessor generation)))

(defmethod initialize-properties :after ((o generational-evolver))
  "Initialize <o> properties."
  (add-properties-from-values
   o 
   (:name 'max-generations :label "Max generations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :editor 'integer-editor :default-value 100)
   (:name 'generation :label "Generation" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :editor 'integer-editor :default-value 0)))

(defmethod evolution-phase ((e generational-evolver))
  (generation e))

(defmethod test-termination ((a configurable-search-algorithm) (e generational-evolver))
  (or (>= (generation e) (max-generations e))
      (and (max-evaluations a) 
           (> (evaluations (fitness-evaluator a))
              (max-evaluations a)))))

(defmethod increase-evolution-phase ((e generational-evolver))
  "Increase <e> evolution phase counter."
  (incf (generation e)))

;; #TODO: Unhardcode random
(defmethod select-replacement-index (evolver operation object)
  (random-integer 0 (population-size (population evolver))))

(defmethod replace-individual-into ((e generational-evolver) operation object population)
  "Replace <object> into <population> for <operation>."
  (evaluate (algorithm e) object)
  (add-individual population object))

(defmethod breed ((b generational-evolver))
  (let* ((population (population b))
         (size (count-individuals population))
         (new-population (make-instance 'population :count-individuals size)))
    ;; Fill new population
    (dotimes (i size)
      (breed-individual-into b population new-population))
    ;; Replace new population
    (setf (population (algorithm b)) new-population)))


(defclass evolver-iterator (object-with-properties)
  ((evolver :initarg :evolver :accessor evolver)
   (operations :initarg :operations :initform nil :accessor operations)))

(defmethod iterate ((iterator evolver-iterator))
  (dolist (i (operations iterator))
    (execute-specific-operation i iterator)))


(defclass operation-wrapper (base-model)
  ((operation :initarg :operation :accessor operation)))

(defmethod execute-specific-operation ((o operation-wrapper) iterator)
  (funcall (operation o) (evolver iterator)))


;; Other
(defmethod compute-object-interface-pixmap-step ((o configurable-search-algorithm) subtask pixmap width heigth render-precision)
  "Compute pixmap values into <pixmap> of <o>."
  (let* ((bgra-vector (make-array (* heigth width 4) :element-type '(unsigned-byte 8)))
         (bgra (make-array (list heigth width 4) :element-type '(unsigned-byte 8) :displaced-to bgra-vector))
         (image  (gp:make-image-from-port pixmap 0 0 width heigth))
         (access (gp:make-image-access pixmap image)))
    (gp:image-access-pixels-from-bgra access bgra-vector)
    (gp:free-image-access access)
    image))

(defmethod create-child ((o configurable-search-algorithm) algorithm operation parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <o> geneotype."
  (let* ((parent-programs (mapcar (lambda (i) (program i)) parents))
         (new-exp (operate operation (language algorithm) parent-programs)))
    (prepare-children-from o new-exp algorithm)))

(defmethod create-valid-child ((o configurable-search-algorithm) algorithm parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <o> geneotype."
  (let* ((parent-programs (mapcar (lambda (i) (program i)) parents))
         (children (create-valid-expression o (language algorithm) parent-programs)))
    (prepare-children-from o children algorithm)))
