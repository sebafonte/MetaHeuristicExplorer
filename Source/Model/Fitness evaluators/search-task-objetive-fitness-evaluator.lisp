
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
  (let* ((task-description (program object))
         (task-object (eval task-description))
         (task-tree (build-task-tree task-description)))    
    (declare (ignore task-tree))
    (execute-search task-object)
    (setf (fitness object) (fitness (best-individual task-object)))))


;;; #TODO: Tests for task description evaluation
(defmethod node-result (node-best-object)
  nil)

(defmethod node-result (search-task)
  nil)

(defun build-task-tree (task-description)
  nil)
  

