
(defclass language (object-with-properties)
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (simplification-function :initarg :simplification-function :accessor simplification-function)
   (valid-new-expresion-function :initarg :valid-new-expresion-function :accessor valid-new-expresion-function)
   (operators :initarg :operators :accessor operators)))


(defmethod initialize-properties :after ((o language))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type :default-value nil :data-type 'symbol 
    :editor 'text-editor)
   (:name 'description :label "Description" :accessor-type 'accessor-accessor-type :default-value "Description"
    :data-type 'string :editor 'text-editor)
   (:name 'valid-new-expresion-function :label "Valid exp function" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'list-editor :default-value 'create-new-parent-copy
    :possible-values (valid-new-expressions-functions))
   (:name 'simplification-function :label "Edit function" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'list-editor :possible-values (simplification-functions))
   (:name 'operators :label "Genetic operators" :accessor-type 'accessor-accessor-type 
    :data-type 'list-structure :default-value nil :editor 'object-list-probability-editor
    :setter '(setf operators))))

(defmethod (setf operators) (operators (l language))
  (setf (slot-value l 'operators) (normalize-operation-list operators)))

(defmethod languages-compatible ((a language) (b language))
  (equal (name a) (name b)))

(defmethod print-object ((o language) seq)
  (format seq "~A" (name o)))

(defmethod simplify ((l language) expression)
  (let ((simplification-function (simplification-function l)))
    (if simplification-function
        (apply simplification-function (list expression nil l))
      expression)))

(defun valid-new-expressions-functions ()
  '(create-new-random-valid
    create-new-first-parent-copy
    create-new-first-parent-program-copy))

(defun simplification-functions ()
  '(simplify-strategy
    simplify-polynomial
    simplify-texture-deformation-separate
    nil))

(defmethod create-new-random-valid ((l language) parents)
  (declare (ignore parents))
  (let* ((max-size (max-size-new-individuals l))
         (max-depth (max-depth-new-individuals l))
         (new-random-expression (create-expresion l max-size max-depth t t)))
    (simplify l new-random-expression)))

(defmethod create-new-first-parent-program-copy ((l language) parents)
  (copy (program (first parents))))

(defmethod create-new-first-parent-copy ((l language) parents)
  (copy (first parents)))

(defmethod copy ((o language))
  (let ((copy (copy-instance o))
        (new-operators (funcall (ttrav #'cons (lambda (x) (copy x))) (operators o))))
    (setf (operators copy) new-operators)
    copy))

(defmethod create-random-from-production ((l language) terminal max-size weight-function)
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

(defmethod update-language ((o language))
  "Update <o> internal state."
  nil)

(defmethod arity-token ((l language) word)
  (arity-token (language grammar) word))
