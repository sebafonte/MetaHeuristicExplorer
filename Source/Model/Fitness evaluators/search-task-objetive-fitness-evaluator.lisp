
(defclass search-task-objetive-fitness-evaluator (entity-evaluator)
  ((candidate-property-name :initarg :candidate-property-name :accessor candidate-property-name)
   (number-of-samples :initarg :number-of-samples :accessor number-of-samples)
   (candidate-object-class :initarg :candidate-object-class :accessor candidate-object-class)
   (candidate-language :initarg :candidate-language :accessor candidate-language)
   (candidate-fitness-evaluator :initarg :candidate-fitness-evaluator :accessor candidate-fitness-evaluator)))


(defmethod initialize-properties :after ((o search-task-objetive-fitness-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'candidate-property-name :label "Candidate property" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'fitness :editor 'integer-editor)
   (:name 'number-of-samples :label "Samples" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 10 :editor 'number-editor)
   (:name 'candidate-object-class :label "Candidate object class" :accessor-type 'accessor-accessor-type :category "Objetive"
    :data-type 'symbol :default-value (default-search-object-class) :possible-values (possible-classes-to-search) 
    :editor 'list-editor)
   (:name 'candidate-language :label "Language" :accessor-type 'accessor-accessor-type :category "Objetive"
    :data-type 'model :editor 'configurable-copy-list-editor 
    :dependency (make-eql-language-dependence 'candidate-object-class)
    :default-value-function (lambda (objetive-class) (copy-cyclic (default-language (make-instance objetive-class))))
    :possible-values-function (lambda (objetive-class) (possible-languages (make-instance objetive-class))))
   (:name 'candidate-fitness-evaluator :label "Fitness evaluator" :accessor-type 'accessor-accessor-type 
    :editor 'configurable-copy-list-editor :category "Objetive" :data-type 'model
    :dependency (make-possible-class-dependency 'candidate-object-class)
    :default-value-function (lambda (objetive-class) (copy-cyclic (first (default-fitness-evaluators (make-instance objetive-class)))))
    :possible-values-function (lambda (objetive-class) (default-fitness-evaluators (make-instance objetive-class))))))

(defmethod evaluate ((evaluator search-task-objetive-fitness-evaluator) (object search-task))
  "Use <evaluator> to calculate and answer <object> fitness."
  (let ((task-description (cadr-insert (program object) object)))
    (multiple-value-bind (result task)
        (eval task-description)
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
