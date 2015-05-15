
(defclass entity (base-model)
  ((gen :initarg :gen :initform nil :accessor gen)
   ;; Fitness real
   (fitness :initarg :fitness :initform 0 :accessor fitness)
   ;; Fitness auxiliares para los cálculos (dependen de la población)
   (fitness-adjusted :initarg :fitness-adjusted :initform :fitness-adjusted :accessor fitness-adjusted)
   (fitness-normalized :initarg :fitness-normalized :initform :fitness-normalized :accessor fitness-normalized)))


(defmethod initialize-instance :after ((o entity) &key expresion) 
  "Initialize <o>."
  (setf (gen o) (make-instance 'genotype :expresion expresion)))

(defmethod ephemeral-properties-definition ((o entity))
  "Answer <o> ephemeral properties."
  (property-from-values-list
   o
   (:name 'program :label "Program" :accessor-type 'valuable-accessor-type 
    :data-type 'list :editor 'list-editor :read-only t :getter 'program-text :visible nil)
   (:name 'fitness :label "Fitness" :accessor-type 'accessor-accessor-type :read-only t :visible nil
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 10000 :editor 'number-editor)))

(defmethod lambda-default-fitness-comparer ((a entity) (b entity))
  (> (fitness a) (fitness b)))

(defmethod lambda-default-fitness-value-comparer ((a entity) fitness-value)
  (> (fitness a) fitness-value))

(defmethod program ((o entity))
  "Answer <o> program."
  (if (gen o) (expresion (gen o))))

(defmethod (setf program) (value (o entity))
  "Set <program> to <o>."
  (setf (expresion (gen o)) value))

(defmethod (setf fitness) (value (o entity))
  "Set <o> fitness."
  (setf (fitness (gen o)) value))

(defmethod fitness ((o entity))
  "Answer <o> fitness."
  (fitness (gen o)))

(defmethod fitness-inverse ((o entity))
  "Answer <o> negated fitness."  
  (- (fitness-max o) (fitness o)))

(defmethod fitness-max ((o entity))
  "Answer <o> maximum possible fitness value."
  10)

(defmethod possible-classes-to-search ()
  "Answer the possible classes to search."
  (sort (concrete-subclasses 'entity) 'string<))

(defmethod evolvablep ((o entity))
  "Answer whether <o> can be evolved."
  t)

(defmethod equivalent ((a entity) (b entity) &optional &key check-genotype check-phenotype criteria)
  "Answer whether <a> and <b> are equivalent."
  (declare (ignore check-genotype)
           (ignore check-phenotype)
           (ignore criteria))
  (equal (program a) (program b)))

(defmethod constant-p ((o entity) &optional (check-genotype t) (check-phenotype t))
  "Answer whether <o> is constant."
  (error "Subclass responsibility."))

(defmethod abstractp ((o (eql 'entity)))
  "Answer whether <o> is abstract."
  t)

(defmethod structure-size ((o entity))
  "Answer the gene structure size of <o>."
  (tree-size (program o)))

(defmethod published-actions ((o entity))
  "Answer the published actions for <o>."
  nil)

(defmethod default-language ((o entity))
  (first (possible-languages o)))

(defmethod possible-languages ((o entity))
  nil)

(defmethod save-to-file ((o entity) file-path &optional &key (tag "#base-model"))
  "Saves <o> into a file with <file-path>."
  (with-open-file (ostream file-path :direction :output :if-exists :append :if-does-not-exist :create)
    (format ostream tag)
    (print (program o) ostream)
    (format ostream "~%")))

(defmethod equals ((a entity) (b entity))
  (equals (program a) (program b)))

;; #TODO: REFACTOR
(defmethod prepare-children-from ((o entity) exp algorithm)
  "Prepares <o> to behave like <exp>."
  (setf (program o) (prepare-children-language (language algorithm) exp)))

