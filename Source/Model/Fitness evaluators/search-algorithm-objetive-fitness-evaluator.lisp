
(defclass search-algorithm-objetive-fitness-evaluator (entity-evaluator)
  ((samples :initarg :samples :accessor samples)
   (candidate-property-name :initarg :candidate-property-name :accessor candidate-property-name)
   (candidate-object-class :initarg :candidate-object-class :accessor candidate-object-class)
   (candidate-language :initarg :candidate-language :accessor candidate-language)
   (candidate-fitness-evaluator :initarg :candidate-fitness-evaluator :accessor candidate-fitness-evaluator)
   (timeout :initarg :timeout :accessor timeout)))


(defmethod initialize-properties :after ((o search-algorithm-objetive-fitness-evaluator))
  "Initialize <o> properties."
  (let ((candidate-object-class (default-search-object-class o)))
    (add-properties-from-values
     o
     ;; Evaluation specific properties
     (:name 'candidate-object-class :label "Candidate object class" :accessor-type 'accessor-accessor-type 
      :data-type 'symbol :default-value candidate-object-class :possible-values (possible-classes-to-search) 
      :editor 'list-editor :update-callback 'lambda-update-callback-search-algorithm-objetive-fitness-evaluator
      :setter '(setf candidate-object-class))
     (:name 'candidate-property-name :label "Candidate property" :accessor-type 'accessor-accessor-type 
      :editor 'symbol-editor :data-type 'symbol :default-value 'fitness)
     (:name 'samples :label "Samples" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :default-value 10 :editor 'number-editor)
     (:name 'timeout :label "Timeout" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :default-value 15 :editor 'number-editor)
     ;; Evaluation context (auxliliary in some way) parameters
     ;; #TODO: I think here a new event :task-language-changed should be implemented 
     (:name 'candidate-language :label "Candidate language" :accessor-type 'accessor-accessor-type 
      :data-type 'model :editor 'configurable-copy-list-editor 
      :dependency (make-possible-class-dependency 'candidate-object-class)
      :default-value-function (lambda (objetive-class) (copy (default-language (make-instance objetive-class))))
      :possible-values-function (lambda (objetive-class) (copy-tree (possible-languages (make-instance objetive-class)))))
     (:name 'candidate-fitness-evaluator :label "Candidate fitness evaluator" :accessor-type 'accessor-accessor-type 
      :data-type 'model :editor 'configurable-copy-list-editor 
      :dependency (make-possible-class-dependency 'candidate-object-class)
      :default-value-function (lambda (objetive-class) (first (default-fitness-evaluators (make-instance objetive-class))))
      :possible-values-function (lambda (objetive-class) (default-fitness-evaluators (make-instance objetive-class)))))))

(defun lambda-update-callback-search-algorithm-objetive-fitness-evaluator (object property) 
  (declare (ignore property)))

(defmethod default-search-task-for-algorithm ((e search-algorithm-objetive-fitness-evaluator) algorithm)
  (let ((task (make-instance 'search-task)))
    (setf (objetive-class task) (candidate-object-class e)
          (algorithm task) algorithm
          (context algorithm) task
          (language task) (candidate-language e)
          (fitness-evaluator task) (candidate-fitness-evaluator e))
    task))

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
      (execute-subtask-local (system-get 'global-coevolution-running-image-planifier) task)
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
