
(defclass search-task-objetive-fitness-evaluator (entity-evaluator)
  ((candidate-property-name :initarg :candidate-property-name :accessor candidate-property-name)
   (number-of-samples :initarg :number-of-samples :accessor number-of-samples)))


(defmethod initialize-properties :after ((o search-task-objetive-fitness-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'candidate-property-name :label "Candidate property" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'fitness :editor 'integer-editor)
   (:name 'number-of-samples :label "Samples" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 10 :editor 'number-editor)))

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
