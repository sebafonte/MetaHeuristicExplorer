
(defclass tree-language (language)
  ((max-size :initarg :max-size :initform 16 :accessor max-size)
   (max-depth :initarg :max-depth :initform 8 :accessor max-depth)
   (max-depth-new-individuals :initarg :max-depth-new-individuals :initform 5 :accessor max-depth-new-individuals)
   (max-size-new-individuals :initarg :max-size-new-individuals :initform 10 :accessor max-size-new-individuals)
   (max-depth-crossover-individuals 
    :initarg :max-depth-crossover-individuals :initform 4 :accessor max-depth-crossover-individuals)
   (max-depth-mutated-individuals 
    :initarg :max-depth-mutated-individuals :initform 4 :accessor max-depth-mutated-individuals)
   (max-depth-mutated-subtree :initarg :max-depth-mutated-subtree :initform 3 :accessor max-depth-mutated-subtree)
   (simplification-patterns :initarg :simplification-patterns :accessor simplification-patterns)
   (constants-strategy :initarg :constants-strategy :accessor constants-strategy)
   (functions :initarg :functions :initform nil :accessor functions)
   (variables :initarg :variables :initform nil :accessor variables)
   (terminals :initarg :terminals :initform nil :accessor terminals)
   (tokens :initarg :tokens :initform nil :accessor tokens)))


(defmethod initialize-properties :after ((o tree-language))
  "Initialize <o> properties."
  (let ((factories (default-constant-factory-symbolic-regression)))
    (add-properties-from-values
     o
     ;; Properties independent of <o>
     (:name 'max-size :label "Max size" :accessor-type 'accessor-accessor-type :object-parameter t
      :data-type 'integer :min-value 1 :max-value 10000 :default-value 18 :editor 'number-editor)
     (:name 'max-depth :label "Max depth" :accessor-type 'accessor-accessor-type :object-parameter t
      :data-type 'integer :min-value 1 :max-value 10000 :default-value 8 :editor 'number-editor )
     (:name 'max-size-new-individuals :label "Initial max size" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :min-value 1 :max-value 10000 :default-value 10 :editor 'number-editor :object-parameter t)
     (:name 'max-depth-new-individuals :label "Initial max depth" :accessor-type 'accessor-accessor-type
      :data-type 'integer :min-value 1 :max-value 10000 :default-value 4 :editor 'number-editor :object-parameter t)
     ;; Constant creation strategy
     (:name 'constants-strategy :label "Constants" :accessor-type 'accessor-accessor-type
      :data-type 'object :possible-values factories :default-value (first factories) 
      :editor 'configurable-copy-list-editor)
     (:name 'simplification-patterns :label "Edit patterns" :accessor-type 'accessor-accessor-type 
      :data-type 'list :editor 'list-editor :default-value nil)
     ;; Properties dependent of <o>
     (:name 'functions :label "Functions" :accessor-type 'accessor-accessor-type 
      :data-type 'list-structure :editor 'one-line-lisp-editor)
     (:name 'variables :label "Variables" :accessor-type 'accessor-accessor-type 
      :data-type 'list :editor 'one-line-lisp-expresion-editor :visible nil)
     (:name 'terminals :label "Terminals" :accessor-type 'accessor-accessor-type 
      :data-type 'list-structure :editor 'lisp-editor))))

(defmethod max-size-for ((l tree-language) evaluator)
  (max-size l))

;; #TODO: Refactor, temporal thing, must use position in tree while moving into it
(defmethod function-symbol-p (node (language tree-language))
  "Answer wheter <node> represent a <language> function."
  (not (null (assoc node (functions language)))))

(defmethod node-language-variable-p (node (language tree-language))
  "Answer wheter <node> represent a <language> variable."
  (block nil
    (dolist (var (variables language))
      (if (eq var node) (return t)))))

(defmethod node-variable-p (node (language tree-language))
  "Answer wheter <node> is variable for <language>."
  (node-language-variable-p node language))

(defmethod node-constant-p ((node number) language)
  "Answer wheter <node> is a <language> constant."
  t)

(defmethod node-constant-p ((node image-vector-3d) language)
  "Answer wheter <node> is a <language> constant."
  t)

(defmethod node-constant-p ((node t) language)
  "Answer wheter <node> is a <language> constant."
  (node-constant-p (constants-strategy language) node))

(defmethod subexp-constant-p (exp (language tree-language))
  "Answer wheter <exp> is constant for <language>."
  (not 
   (if (atom exp)
       (node-variable-p exp language)
     (subexp-variable-p exp language))))

(defmethod subexp-variable-p (exp language)
  "Answer whether <exp> is variable for <language>."
  (if (atom exp) 
      (node-variable-p exp language)
    (if (subexp-variable-p (car exp) language) 
        t
      (subexp-variable-p (cdr exp) language))))
