
(defclass tree-language (language)
  ((max-size :initarg :max-size :initform 16 :accessor max-size)
   (max-depth :initarg :max-depth :initform 8 :accessor max-depth)
   (max-depth-new-individuals :initarg :max-depth-new-individuals :initform 5 :accessor max-depth-new-individuals)
   (max-size-new-individuals :initarg :max-size-new-individuals :initform 10 :accessor max-size-new-individuals)
   (simplification-function :initarg :simplification-function :accessor simplification-function)
   (simplification-patterns :initarg :simplification-patterns :accessor simplification-patterns)
   (constants-strategy :initarg :constants-strategy :accessor constants-strategy)
   (functions :initarg :functions :initform nil :accessor functions)
   (variables :initarg :variables :initform nil :accessor variables)
   (terminals :initarg :terminals :initform nil :accessor terminals)
   (tokens :initarg :tokens :initform nil :accessor tokens)
   (min-function-args :initarg :min-function-args :accessor min-function-args)))


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
     (:name 'simplification-function :label "Edit function" :accessor-type 'accessor-accessor-type 
      :data-type 'symbol :editor 'list-editor :possible-values (simplification-functions))
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

(defmethod node-language-variable-p (node (l tree-language))
  "Answer wheter <node> represent a <language> variable."
  (block nil
    (dolist (var (variables l))
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

(defmethod subexp-constant-p (exp (l tree-language))
  "Answer wheter <exp> is constant for <language>."
  (not 
   (if (atom exp)
       (node-variable-p exp l)
     (subexp-variable-p exp l))))

(defmethod subexp-variable-p (exp language)
  "Answer whether <exp> is variable for <language>."
  (if (atom exp) 
      (node-variable-p exp language)
    (if (subexp-variable-p (car exp) language) 
        t
      (subexp-variable-p (cdr exp) language))))

(defmethod simplify ((l tree-language) exp)
  (let ((simplification-function (simplification-function l)))
    (if simplification-function
        (apply simplification-function (list exp nil l))
      exp)))

(defun simplification-functions ()
  '(simplify-strategy
    simplify-polynomial
    simplify-texture-deformation-separate
    nil))

(defmethod create-new-random-valid ((l tree-language) parents)
  (declare (ignore parents))
  (let* ((max-size (max-size-new-individuals l))
         (max-depth (max-depth-new-individuals l))
         (new-random-exp (create-expresion l max-size max-depth t t)))
    (simplify l new-random-exp)))

(defmethod create-random-from-production ((l tree-language) terminal max-size weight-function)
  (if (null weight-function) 
      (setf weight-function #'lambda-weight-equal-random-selection-list))
  (let* ((grammar (grammar l))
         (exp-list terminal)
         (productions (updated-productions grammar)))
    ;; #REFACTOR: Check why it has been done in this way, i is never used
    (do ((i 0))
        ((all-keywords-in exp-list))
      (setf exp-list (replace-random-production grammar exp-list productions max-size weight-function)))
    (de-flatten-parenthesis (generate-random-token-values l exp-list))))

(defun generate-random-token-values (language expression)
  (let ((result))
    (dolist (i expression)
      (appendf result (list (create-random-token language i))))
    result))

(defmethod create-random-token (language (token (eql :var)))
  "Answer a random value for <token>."
  (random-element (variables language)))

(defmethod create-random-token (language (token (eql :constant)))
  "Answer a random value for <token>."
  (create-constant (constants-strategy language)))

(defmethod create-random-token (language (token symbol))
  "Answer a random value for <token>."
  (car (random-element
        (select (tokens (grammar language))
                (lambda (value) 
                  (and (equal token (cadr value))
                       (can-create-token language (car value))))))))

(defmethod can-create-token (language token)
  "Answer whether <language> can create <token> in a new random expression." 
  (or (structural-symbol token)
      (find-if (lambda (value) (equal token (car value)))
               (functions language))))

;; #TODO: Error here, see what happened
(defmethod arity-token ((l tree-language) word)
  (arity-token (language grammar) word))

(defun lambda-weight-for-index-random-selection-list (list position value)
  (declare (ignore list) (ignore value))
  (1+ position))

(defun lambda-weight-equal-random-selection-list (list position value)
  (declare (ignore list) (ignore position) (ignore value))
  1)

(defun lambda-weight-heuristic-1-random-selection-list (list position value)
  "Select with index selection prioring to productions with more elements."
  (declare (ignore list) (ignore position))
  (tree-size (cdar value)))

(defun lambda-weight-heuristic-2-random-selection-list (list position value)
  "Always select the one with maximum elements."
  (declare (ignore position))
  (let* ((production-core (cdar value))
         (max-size (tree-size (cdar (maximum-of list (lambda (value) (tree-size value)))))))
    (if (equal (tree-size production-core) max-size)
        1
      0)))

(defmethod prepare-children-language ((l tree-language) exp)
  (simplify l exp))

(defun min-language-function-with-args (x)
  (reduce 'min (mapcar 'cadr x)))

(defmethod get-key ((obj tree-language))
  (format nil "~A-~A" 'tree-language (variables obj)))

#|
;; #NOTE: not used, there is a more specific version in fixes.lisp for 'cfg-tree-language
(defmethod (setf functions) (tree (o tree-language))
  (setf (min-function-args o) (min-language-function-with-args (functions o))))
|#
