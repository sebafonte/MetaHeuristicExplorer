(defclass object-in-search (base-model)
  ((object :initarg :object :initform nil :accessor object)
   (context :initarg :context :initform nil :accessor context)))


(defmethod ephemeral-properties-definition ((o object-in-search))
  "Answer <o> ephemeral properties."
  (property-from-values-list
   o
   (:name 'program :label "Program" :accessor-type 'valuable-accessor-type 
    :data-type 'list :editor 'lisp-editor :read-only t :getter 'program-text)
   (:name 'size :label "Size" :accessor-type 'valuable-accessor-type 
    :data-type 'integer :read-only t :editor 'number-editor :getter 'structure-size)
   (:name 'fitness :label "Fitness" :accessor-type 'accessor-accessor-type :read-only t
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 10000 :editor 'number-editor)
   (:name 'object :label "Object" :accessor-type 'accessor-accessor-type :read-only t
    :data-type 'object :editor 'button-editor)
   (:name 'context :label "Context" :accessor-type 'accessor-accessor-type :read-only t
    :data-type 'object :editor 'button-editor)))

(defmethod structure-size ((o object-in-search))
  "Answer the gene structure size of <o>."
  (structure-size (object o)))

(defun program-text (o)
  (format nil "~A" (if o (program o))))

(defmethod apply-changes ((o object-in-search))
  "Update <o> for any possible change."
  (if (context o)
      (evaluate (algorithm (context o)) (object o))))

(defmethod copy ((o object-in-search))
  (make-instance 'object-in-search :object (copy (object o)) :context (context o)))

(defmethod evolvablep ((o object-in-search))
  "Answer whether <o> can be evolved."
  t)

(defmethod drawablep ((o object-in-search))
  "Answer whether <o> can be displayed on the GUI."
  t)

(defmethod constant-p ((o object-in-search) &optional (check-genotype t) (check-phenotype t))
  "Answer whether <o> is constant."
  (constant-p (object o) check-genotype check-phenotype))

(defmethod program ((o object-in-search))
  "Answer <o> program."
  (program (object o)))

(defmethod (setf program) (program (o object-in-search))
  "Set <program> to <o>."
  (setf (program (object o)) program))

(defmethod equivalent ((a object-in-search) (b object-in-search) &optional &key check-genotype check-phenotype criteria)
  (equivalent (object a) (object b)
               :check-genotype check-genotype
               :check-phenotype check-phenotype
               :criteria criteria))

(defmethod fitness ((o object-in-search))
  (fitness (object o)))

(defmethod language ((o object-in-search))
  (language (context o)))