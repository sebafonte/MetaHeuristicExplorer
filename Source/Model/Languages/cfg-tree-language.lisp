
(defclass cfg-tree-language (tree-language)
  ((grammar :initarg :grammar :accessor grammar)
   (specialized-tokens :initarg :specialized-tokens :initform nil :accessor specialized-tokens)))


(defmethod initialize-instance :after ((o cfg-tree-language) &rest args)
  "Initialize <o>."
  (initialize-grammar o)
  o)

(defmethod (setf grammar) (value (o cfg-tree-language))
  (setf (slot-value o 'grammar) value)
  (initialize-grammar o))

(defmethod initialize-grammar ((o cfg-tree-language))
  (when (and (slot-boundp o 'grammar) (grammar o))
    (set-grammar-tokens o)
    (set-grammar-productions o)
    (calculate-minimum-production-size (grammar o))))

(defmethod set-grammar-tokens ((o cfg-tree-language))
  (setf (tokens (grammar o))
        (append (variable-tokens o)
                (function-tokens o)
                (auxiliar-tokens o)
                (fixed-constants-tokens o)
                (specialized-tokens o))))

(defmethod set-grammar-productions ((o cfg-tree-language))
  (update-end-productions (grammar o) (tokens o) (functions o) (variables o) (specialized-tokens o)))

(defmethod initialize-properties :after ((o cfg-tree-language))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'grammar :label "Grammar" :accessor-type 'accessor-accessor-type
    :data-type 'object :editor 'button-editor)))

(defmethod variable-tokens ((o cfg-tree-language))
  (mapcar (lambda (value) (list value :var))
          (variables o)))

(defmethod function-tokens ((o cfg-tree-language))
  (tokens o))

(defmethod auxiliar-tokens ((o cfg-tree-language))
  '((:open :open)
    (:close :close)))

(defmethod fixed-constants-tokens ((o cfg-tree-language))
  nil)

(defmethod create-random-token ((language cfg-tree-language) (token (eql :string)))
  "Answer a random value for <token>."
  (symbol-name (gensym)))

(defmethod create-random-token ((language cfg-tree-language) (token (eql :symbol)))
  "Answer a random value for <token>."
  (gensym))

(defmethod is-correct ((language cfg-tree-language) program)
  "Answer whether <program> is correct for <language>."
  (multiple-value-bind (parse-tree result)
      (parse (grammar language) program)
    (not result)))

(defmethod compatible-language ((a cfg-tree-language) (b cfg-tree-language))
  (and (equals (variables a) (variables b))
       (equals (tokens a) (tokens b))))