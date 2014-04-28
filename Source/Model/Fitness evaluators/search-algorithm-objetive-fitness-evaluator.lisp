
(defclass search-algorithm-objetive-fitness-evaluator (entity-evaluator)
  ((samples :initarg :samples :accessor samples)
   (candidate-property-name :initarg :candidate-property-name :accessor candidate-property-name)
   (candidate-object-class :initarg :candidate-object-class :accessor candidate-object-class)
   (candidate-language :initarg :candidate-language :accessor candidate-language)
   (candidate-fitness-evaluator :initarg :candidate-fitness-evaluator :accessor candidate-fitness-evaluator)
   (timeout :initarg :timeout :accessor timeout)))


(defmethod initialize-instance :after ((o search-algorithm-objetive-fitness-evaluator) &rest initargs)
  "Initialize <object>."
  (declare (ignore initargs))
  (reset-specific-properties o)  
  (reset-with-valid-dependences o))

(defmethod initialize-properties :after ((o search-algorithm-objetive-fitness-evaluator))
  "Initialize <o> properties."
  (let ((candidate-object-class (default-search-object-class o)))
    (add-properties-from-values
     o
     ;; Evaluation specific properties
     (:name 'candidate-object-class :label "Candidate object class" :accessor-type 'accessor-accessor-type 
      :data-type 'symbol :default-value candidate-object-class :possible-values (possible-classes-to-search) 
      :editor 'list-editor :update-callback 'lambda-update-callback-search-algorithm-objetive-fitness-evaluator
      :setter '(setf candidate-object-class) :subject o)
     (:name 'candidate-property-name :label "Candidate property" :accessor-type 'accessor-accessor-type 
      :editor 'symbol-editor :data-type 'symbol :default-value 'fitness :subject o)
     (:name 'samples :label "Samples" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :default-value 10 :editor 'number-editor :subject o)
     (:name 'timeout :label "Timeout" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :default-value 15 :editor 'number-editor :subject o))))

(defmethod initialize-properties-for ((o t) (target search-algorithm-objetive-fitness-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   target
   ;; Evaluation context (auxliliary in some way) parameters
   (:name 'candidate-language :label "Candidate language" :accessor-type 'accessor-accessor-type 
    :data-type 'object :default-value (copy (default-language (objetive-instance target))) 
    :editor 'configurable-copy-list-editor 
    :possible-values (copy-tree (possible-languages (objetive-instance target))) :subject o)
   (:name 'candidate-fitness-evaluator :label "Candidate fitness evaluator" :accessor-type 'accessor-accessor-type 
    :data-type 'object :default-value (first (default-fitness-evaluators (objetive-instance target)))
    :editor 'configurable-copy-list-editor 
    :possible-values (default-fitness-evaluators (objetive-instance target)) :subject o)))

(defmethod (setf candidate-object-class) (value (o search-algorithm-objetive-fitness-evaluator))
  (let ((old-value (candidate-object-class o)))
    (setf (slot-value o 'candidate-object-class) value)
    (when (not (equal old-value (candidate-object-class o)))
      (reset-with-valid-dependences o))))

(defmethod reinitialize-fitness-evaluator ((o search-algorithm-objetive-fitness-evaluator))
  "Reinitialices possible fitness evaluators for <o>."
  (let* ((property (property-named o 'candidate-fitness-evaluator))
         (default-fitness-evaluators (default-fitness-evaluators (objetive-instance o)))
         (default-fitness-evaluator (first default-fitness-evaluators)))
    (setf (possible-values property) default-fitness-evaluators
          (default-value property) default-fitness-evaluator)
    (set-default-property-value o property)))

(defmethod reset-specific-properties ((o search-algorithm-objetive-fitness-evaluator))
  (re-initialize-properties-for (objetive-instance o) o)
  (set-default-property-values o))

(defun lambda-update-callback-search-algorithm-objetive-fitness-evaluator (object property) 
  (declare (ignore property))
  (reset-specific-properties object))

(defmethod default-search-task-for-algorithm ((e search-algorithm-objetive-fitness-evaluator) algorithm)
  (let ((task (make-instance 'search-task)))
    (setf (objetive-class task) (candidate-object-class e)
          (algorithm task) algorithm
          (context algorithm) task
          (language task) (candidate-language e)
          (fitness-evaluator task) (candidate-fitness-evaluator e))
    task))

(defmethod objetive-instance ((o search-algorithm-objetive-fitness-evaluator))
  (make-instance (candidate-object-class o)))

(defmethod evaluate ((evaluator search-algorithm-objetive-fitness-evaluator) (object configurable-search-algorithm))
  "Use <evaluator> to calculate and answer <object> fitness."
  (let* ((grammar (system-get 'search-algorithm-grammar))
         (expression (parse grammar (program object)))
         (expression-fixed (compress-flatten-parenthesis-token-value expression))
         (algorithm (eval (replace-label-cadr :list-auxiliar expression-fixed)))
         (best-individual)
         (task (default-search-task-for-algorithm evaluator algorithm)))
    (setf (evolver object) (evolver algorithm)
          (population object) (population algorithm)
          (population-size object) (population-size algorithm)
          (elite-manager object) (elite-manager algorithm)
          (initializer object) (initializer algorithm)
          (language object) (language algorithm))
    (dotimes (i (samples evaluator))
      (execute-subtask-local (system-get 'global-running-image-planifier) task)
      (mp:process-wait-with-timeout
       "Evaluating task..."
       (timeout evaluator)
       (lambda (object) (equal (state task) 'finished))
       task)
      (setf (population object) (population algorithm))
      (let ((best (best-individual (population algorithm))))
        (if (or (not best-individual)
                (better-than best best-individual))
            (setf best-individual best))))
    ;; #TODO: Debug
    (setf (context object) task)
    (setf (fitness object) (fitness best-individual))
    (setf (fitness (gen object)) (fitness best-individual))))
    
(defmethod specialize-language ((task search-task) (evaluator search-algorithm-objetive-fitness-evaluator))
  (specialize-language-from (language task) (candidate-language evaluator)))

(defmethod specialize-language-from ((language cfg-tree-language) (candidate language))
  (let ((unary (candidate-language-unary-operation-names candidate))
        (binary (candidate-language-binary-operation-names candidate)))
    (setf (specialized-tokens language) 
          (append
           (mapcar (lambda (o) (list o :unary-operation-name)) unary)
           (mapcar (lambda (o) (list o :binary-operation-name)) binary)))
    (initialize-grammar language)))

(defmethod candidate-language-unary-operation-names ((language language))
  (candidate-language-n-operation-names language 1))

(defmethod candidate-language-binary-operation-names ((language language))
  (candidate-language-n-operation-names language 2))

(defmethod candidate-language-n-operation-names ((language language) n)
  (mapcar (lambda (o) (intern (name (car o)) :keyword))
          (select
           (operators language)
           (lambda (o)
             (and (= n (arity (system-get (intern (name (car o))))))
                  (> (cadr o) 0))))))
