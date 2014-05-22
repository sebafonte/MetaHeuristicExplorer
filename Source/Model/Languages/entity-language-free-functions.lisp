
(defclass entity-language-lisp-functions (entity-language)
  ((functions :initarg :functions :initform nil :accessor functions)
   (variables :initarg :variables :initform nil :accessor variables)
   (terminals :initarg :terminals :initform nil :accessor terminals)
   (max-depth :initarg max-depth :initform 8 :accessor max-depth)
   (max-depth-new-individuals :initarg :max-depth-new-individuals :initform 5 :accessor max-depth-new-individuals)
   (max-size-new-individuals :initarg :max-size-new-individuals :initform 10 :accessor max-size-new-individuals)
   (max-depth-crossover :initarg :max-depth-crossover :initform 4 :accessor max-depth-crossover)
   (max-depth-mutated :initarg :max-depth-mutated :initform 4 :accessor max-depth-mutated)
   (max-depth-mutated-subtree :initarg :max-depth-mutated-subtree :initform 3 :accessor max-depth-mutated-subtree)
   (max-size :initarg :max-size :initform 16 :accessor max-size)
   (constants-strategy :initarg :constants-strategy :accessor constants-strategy)))


(defmethod initialize-properties :after ((o entity-language-lisp-functions))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'functions :label "Functions" :accessor-type 'accessor-accessor-type 
    :data-type 'list-structure :editor 'one-line-lisp-editor :subject o)
   (:name 'variables :label "Variables" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'one-line-lisp-expresion-editor :visible nil :subject o)
   (:name 'terminals :label "Terminals" :accessor-type 'accessor-accessor-type 
    :data-type 'list-structure :editor 'lisp-editor :subject o)
   (:name 'min-size :label "Min size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 1 :editor 'number-editor)
   (:name 'max-size :label "Max size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor)
   (:name 'min-depth :label "Min depth" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 1 :editor 'number-editor)
   (:name 'max-depth :label "Max depth" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor)
   (:name 'constants-strategy :label "Constants" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value (system-get-copy 'default-fixed-set-numerical-1) :editor 'list-editor)
   (:name 'max-depth-mutated :label "Max depth mutated child" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor)
   (:name 'max-depth-mutated-subtree :label "Max depth mutated subtree" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor)))


(defclass entity-language-lisp-functions-grammar (entity-language-lisp-functions)
  ((grammar :initarg :grammar :accessor grammar)))


(defmethod initialize-properties :after ((o entity-language-lisp-functions-grammar))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'grammar :label "Grammar" :accessor-type 'accessor-accessor-type 
    ;:default-value (grammar-default entity) :possible-values (possible-grammars entity) 
    :data-type 'grammar :editor 'configurable-copy-list-editor :subject p)))
