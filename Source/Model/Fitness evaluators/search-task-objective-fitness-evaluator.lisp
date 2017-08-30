
(defclass search-task-objective-fitness-evaluator (entity-evaluator)
  ((candidate-property-name :initarg :candidate-property-name :accessor candidate-property-name)
   (number-of-samples :initarg :number-of-samples :accessor number-of-samples)
   (candidate-object-class :initarg :candidate-object-class :accessor candidate-object-class)
   (candidate-language :initarg :candidate-language :accessor candidate-language)
   (candidate-fitness-evaluator :initarg :candidate-fitness-evaluator :accessor candidate-fitness-evaluator)
   (context :initarg :context :accessor context)))


(defmethod initialize-properties :after ((o search-task-objective-fitness-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'candidate-property-name :label "Candidate property" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'fitness :editor 'integer-editor)
   (:name 'number-of-samples :label "Samples" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 10 :editor 'number-editor)
   (:name 'candidate-object-class :label "Candidate object class" :accessor-type 'accessor-accessor-type :category "Objective"
    :data-type 'symbol :default-value (default-search-object-class) :possible-values (possible-classes-to-search) 
    :editor 'list-editor)
   (:name 'candidate-language :label "Language" :accessor-type 'accessor-accessor-type :category "Objective"
    :data-type 'model :editor 'configurable-copy-list-editor 
    :dependency (make-eql-language-dependence 'candidate-object-class)
    :default-value-function (lambda (objective-class) (copy-cyclic (default-language (make-instance objective-class))))
    :possible-values-function (lambda (objective-class) (possible-languages (make-instance objective-class))))
   (:name 'candidate-fitness-evaluator :label "Fitness evaluator" :accessor-type 'accessor-accessor-type 
    :editor 'configurable-copy-list-editor :category "Objective" :data-type 'model
    :dependency (make-possible-class-dependency 'candidate-object-class)
    :default-value-function (lambda (objective-class) (copy-cyclic (first (default-fitness-evaluators (make-instance objective-class)))))
    :possible-values-function (lambda (objective-class) (default-fitness-evaluators (make-instance objective-class))))))

(defmethod evaluate ((evaluator search-task-objective-fitness-evaluator) (object search-task))
  "Use <evaluator> to calculate and answer <object> fitness."
  (let ((task-description (cadr-insert (program object) (context evaluator))))
    (multiple-value-bind (result task)
        (eval task-description)
      (declare (ignore task))
      (setf (fitness object) (fitness result))
      (fitness result))))

(defun cadr-insert (exp value &optional (condition t))
  (if exp 
      (append (list (car exp))
              (list value)
              (cdr-insert (cdr exp) value condition))))

(defun cdr-insert (exp value &optional (condition t))
  (mapcar 
   (lambda (o) (if (consp o) (cadr-insert o value condition) o))
   exp))

(defmethod ensure-fitness-data-initialized ((o search-task-objective-fitness-evaluator) algorithm)
  (setf (context o) (context algorithm)))
