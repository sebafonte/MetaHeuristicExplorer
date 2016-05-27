
;;; Web interface
(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-get-languages)) message administrator stream)
  "Answer a string with all the system languages."
  (format stream "lisp-math-function-x lisp-math-function-xy lisp-math-function-xyz lisp-math-function-xyzt rgb-color-images-vector rgb-color-images-vector-time")
  (force-output stream))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-infix-converted)) message administrator stream)
  "Answer infix converted translation for object in <message>."
  (format stream "~A" (infix-coverted-string (first (content message))))
  (force-output stream))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-glsl-converted)) message administrator stream)
  "Answer infix converted translation for object in <message>."
  (format stream "~A" (glsl-coverted-string (first (content message))))
  (force-output stream))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-get-default)) message administrator stream)
  "Answer default properties types and values for object in <message>."
  (let* ((name (first (content message)))
         (properties-description (second (content message)))
         (object (task-get name)))
    (if object 
        (properties-object-description-on object properties-description stream)  
      (format stream "error not found"))
    (force-output stream)))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-create-default)) message administrator stream)
  "Answer a new expression for default object for language in <message>."
  (let* ((language-name (first (content message)))
         (result (program (subject (default-object-for language-name)))))
    (format stream "~A | ~A" result (infix-coverted-string result))
    (force-output stream)))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-create-random)) message administrator stream)
  (let* ((language (copy-cyclic (system-get (first (content message)))))
         (max-size (second (content message))))
    (setf (max-size language) max-size)
    (let ((result (create-random-from-production language '(start) max-size nil)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-crossover)) message administrator stream)
  (let* ((language (copy-cyclic (system-get (first (content message)))))
         (object-a (second (content message)))
         (object-b (third (content message)))
         (max-size (fourth (content message)))
         (operator (system-get 'crossover-cfg))
         (variables (fifth (content message))))
    (setf (max-size language) max-size)
    (let ((result (directed-crossover-cfg object-a object-b language operator)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-mutate)) message administrator stream)
  (let* ((language (copy-cyclic (system-get (first (content message)))))
         (object (second (content message)))
         (operator (system-get 'mutate-cfg))
         (max-size (third (content message)))
         (variables (fourth (content message))))
    (setf (max-size language) max-size)
    (let ((result (mutate-cfg object language operator)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-mutate)) message administrator stream)
  (let* ((language (copy-cyclic (system-get (first (content message)))))
         (object (second (content message)))
         (operator (system-get 'mutate-cfg))
         (max-size (third (content message)))
         (variables (fourth (content message))))
    (setf (max-size language) max-size)
    (let ((result (mutate-cfg object language operator)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-glsl-parse-result)) message administrator stream)
  (let* ((language-name (first (content message)))
         (variables (third (content message)))
         (object (car (second (content message))))
         (language (create-language-from language-name variables)))
    (let ((result (caadr (multiple-value-bind (value error) 
                                 (parse (grammar language) object)
                               (if error nil value)))))
      (format stream "~A" result)
      (force-output stream))))

;; Infix converter (#TEMP)
(defun infix-converter (o buffer)
  (if (consp o)
      (if (or (eql (first o) '+) (eql (first o) '-) (eql (first o) '*) (eql (first o) '/))
          (progn 
            ;; Operator
            (format buffer "(")
            (infix-converter (second o) buffer)
            (infix-converter (first o) buffer)
            (infix-converter (third o) buffer)
            (format buffer ")"))
        ;; Function
        (progn 
          (infix-converter (first o) buffer)
          (format buffer "(")
          (dotimes (i (length (cdr o)))
            (infix-converter (nth i (cdr o)) buffer)
            (when (< i (1- (length (cdr o))))
              (format buffer ",")))
          (format buffer ")")))
    ;; Constant or name
    (format buffer "~4$" (replace-converter o))))

;;  glsl-converter (#TEMP)
(defun glsl-converter (o buffer)
  (let ((exp (parse-glsl)))
    (format buffer exp)))

(defun glsl-shader-functions-float-part-info ()
  '((+ 2) (- 2) (* 2) (/- 2) (sin 1) (cos 1) (sqrt 1) (sqr 1) (abs 1) (sqrt 1)))

(defun glsl-shader-functions-vec3-part-info ()
  '((+ 2) (- 2) (* 2) (/- 2) (sin 1) (cos 1) (sqrt 1) (abs 1) (sqrt 1)))

(defmethod replace-converter ((o (eql '/-)))
  'divideprotected)

(defmethod replace-converter ((o t))
  o)

(defun infix-coverted-string (o)
  (with-output-to-string (stream)
    (infix-converter o stream)))

(defun glsl-coverted-string (o) 
  (with-output-to-string (stream)
    (glsl-converter o stream)))

;; Default objects for testing
(defmethod default-object-for ((o (eql 'lisp-math-function-xy)))
  (random-element
   (list (system-get 'function-xy-default-4)
         (system-get 'function-xy-default-6)
         (system-get 'function-xy-default-7)
         (system-get 'function-xy-default-8)
         (system-get 'function-xy-default-9)
         (system-get 'function-xy-default-10))))

(defmethod default-object-for ((o (eql 'rgb-color-images)))
  (random-element
   (list (system-get 'rgb-default-1)
         (system-get 'rgb-default-2)
         (system-get 'rgb-default-3)
         (system-get 'rgb-default-4)
         (system-get 'rgb-default-5))))

(defmethod default-object-for ((o (eql 'vrp-list-language)))
  (system-get 'DEFAULT-VRP-EVALUATOR))

;; Examples extensions
(defvar *interface-tasks*)

(defmethod initialize-default-web-interface-objects ()
  ;; Create task register
  (setf *interface-tasks* (make-hash-table :test 'equals))
  ;; Create a VRP default task 1
  (let ((task (make-instance 'search-task)))
    (setf (name task) 'default-vrp-cw-task
          (runs (task-builder task)) 1)
    (set-value-for-property-named task 'objective-class 'ENTITY-SAMPLE-VRP)
    (add-task (name task) task))
  ;; Create a VRP default task 2
  (let ((task (make-instance 'search-task)))
    (set-value-for-property-named task 'objective-class 'ENTITY-SAMPLE-VRP)
    (setf (name task) 'default-vrp-pure-ga-task
          (generation-method (initialization-method (algorithm task))) 'random-tour-variable-vehicles
          (runs (task-builder task)) 5)
    (add-task (name task) task))
  ;; Create rgb image difference task
  (let ((task (make-instance 'search-task)))
    (setf (name task) 'default-image-task
          (population-size (algorithm task)) 20
          (max-generations (algorithm task)) 50
          (task-planifier task) (system-get 'global-remote-planifier)
          (runs (task-builder task)) 3
          (fitness-evaluator task) (system-get 'entity-image-similarity-pixel-distance)
          (initialization-method (algorithm task)) (system-get 'random-trees-cfg-initializer))
    (set-value-for-property-named task 'objective-class 'entity-image-rgb)
    (add-task (name task) task)))

(defun properties-object-description-on (object properties-list stream)
  (dolist (i properties-list)
    (let ((property (property-named object i)))
      (format stream
              "~A|~A|~A||" 
              i
              (if property (data-type property))
              (get-value-for-property-named object i)))))

(defun properties-object-description-to (object description)
  (dolist (i description)
    (apply-variation object (read-from-string (second i)) (list (first i)))))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-create-task-using)) message administrator stream)
  (let* ((name (first (content message)))
         (properties-description (second (content message)))
         (scheduler (third (content message)))
         (task (copy-cyclic (task-get name))))
    (setf (name task) (symbol-name (gensym)))
    (properties-object-description-to task properties-description)
    (when scheduler
      (setf (task-planifier task) (system-get scheduler)))
    (add-task (name task) task)
    (execute-task (task-planifier task) task)
    (format stream "Task started|~A" (name task))
    (force-output stream)))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-delete-task)) message administrator stream)
  (let* ((name (first (content message)))
         (task (task-get name)))
    (kill-task task)
    (rem-task (symbol-name name))
    (format stream "Task deleted")
    (force-output stream)))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-get-property-value)) message administrator stream)
  (let* ((name (first (content message)))
         (properties (second (content message)))
         (object (task-get name)))
    (update-task-properties object)
    (dolist (i properties)
      (let ((value))
        (let ((property (property-named object i)))
          (setf value 
                (if property
                    (get-value-for-property-named object i)
                  (funcall i object)))
          (format stream "~A|" value))))
    (force-output stream)))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-get-property-value-2)) message administrator stream)
  (let* ((name (first (content message)))
         (properties (second (content message)))
         (object (task-get name)))
    (update-task-properties object)
    (dolist (i properties)
      (let ((value))
        (let ((property (property-named object i)))
          (setf value 
                (if property
                    (get-value-for-property-named object i)
                  (funcall i object)))
          (format stream "~S|" value))))
    (format stream "~%")
    (force-output stream)))

(defun add-task (key value)
  (setf (gethash key *interface-tasks*) value))

(defun rem-task (key)
  (remhash key *interface-tasks*))

(defun task-get (key)
  (let ((result (gethash key *interface-tasks*)))
    (when (and (null result) (symbolp key)) 
      (setf result (gethash (symbol-name key) *interface-tasks*)))
    (if result result
      (progn 
        (error "Task requested not found.")
        result))))

(defun task-evaluations (o)
  (let ((evaluations 0))
    (if (children o)
        (progn 
          (dolist (i (children o))
            (incf evaluations (task-evaluations i)))
          evaluations)
      (evaluations (fitness-evaluator o)))))

(defun task-best-program-rgb (o)
  (if (first (children o))
      (let ((best (best-individual (first (children o)))))
        (if best 
            (let ((result (program best)))
              (format nil "~A" (infix-coverted-string result)))))))

(defun task-best-program-vrp (o)
  (if (first (children o))
      (let ((best (best-individual (first (children o)))))
        (if best 
            (let ((result (program best)))
              (format nil "~A" result))))))


(defclass remote-task-descriptor (base-model)
  ((host :initarg :host :accessor host)
   (name :initarg :name :accessor name)))


(defmethod task-kill ((o remote-task-descriptor))
  (let ((message (make-instance 'tcp-message 
                                :name 'message-web-interface-delete-task 
                                :content (list (name o)))))
    (with-open-stream
        (stream (comm:open-tcp-stream (ip-address (host o)) (port (host o)) :timeout *tcp-default-timeout* :read-timeout *tcp-default-timeout*))
    (if stream
        (progn
          (write-line (transportable-code-description message) stream)
          (force-output stream)
          (let ((result (read-line stream nil nil)))
            ;; #TODO: Process return code
            ;;(if result (read-from-string result))
            ))
      (signal-error-on planifier target task "Closed connection")))))

(defun update-task-properties (task)
  (when (eql (state task) 'running-remote)
    ;; When running remote, query remote host for fitness and evaluation values
    (let ((message (make-instance 'tcp-message 
                                  :name 'message-web-interface-get-property-value-2 
                                  :content (list
                                              (name task)
                                              (list 'fitness 'task-evaluations 'best-individual)))))
      (with-open-stream
          (stream (comm:open-tcp-stream
                   (ip-address (host (process task))) 
                   (port (host (process task))) 
                   :timeout *tcp-default-timeout*))
        (if stream
            (progn
              (write-line (transportable-code-description message) stream)
              (force-output stream)
                (let ((result (split-sequence "|" (read-line stream nil nil))))
                  (setf (fitness task) (first result)
                        (evaluations (fitness-evaluator task)) (second result))))
          (handle-transfer-error "Closed connection"))))
    ;; Update child
    (dolist (i (children task))
      (update-task-properties i))))

(defmethod check-remote-model-update ((o t))
  nil)

(defmethod check-remote-model-update ((o search-task))
  (update-task-properties o))

(defun create-lisp-cfg-language (functions variables constants)
  (let ((grammar (make-instance 'context-free-grammar
                                 :name 'generic-tree-cfg-grammar 
                                 :lexer 'lisp-math-expression-lexer
                                 :parser-initializer 'initialize-lisp-math-expression-parser
                                 :productions (lisp-math-grammar-productions)
                                 :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :expresion))))
    (make-instance 'cfg-tree-language 
                   :name 'generic-tree-cfg-language
                   :description "Generic tree CFG"
                   :grammar grammar
                   :simplification-patterns *lisp-math-expression-simplification-patterns*
                   :constants-strategy constants
                   :functions (entity-function-default-functions-info)
                   :terminals (append variables (list :constant))
                   :variables variables
                   :tokens *lisp-math-expression-tokens*
                   :valid-new-expresion-function 'create-new-random-valid
                   :simplification-function 'simplify-strategy
                   :operators (default-genetic-operators-probability-lisp-expression))))

#|
(defmethod dispatch-message-name ((message-name (eql 'message-prepare-lisp-cfg-language)) message administrator stream)
  (let* ((functions (first (content message)))
         (variables (second (content message)))
         (constants (third (content message)))
         (language (create-lisp-cfg-language functions variables constants)))
    (setf (name language) (symbol-name (gensym)))
    (system-add language)
    (format stream "~A" (name language))
    (force-output stream))) 

(make-instance 'tcp-message :name (quote message-create-random-lisp-cfg-language
) :content (list (quote nil) (quote default-fixed-set-numerical-1) (list (quote
x)  (quote y)  (quote time)) 30))
|#


(defmethod dispatch-message-name ((message-name (eql 'message-create-random-lisp-cfg-language)) message administrator stream)
  (let* ((functions (first (content message)))
         (constants (system-get (second (content message))))
         (variables (third (content message)))
         (max-size (fourth (content message)))
         (language (create-lisp-cfg-language
                    (or functions (entity-function-default-functions-info))
                    variables 
                    constants)))
    (setf (max-size language) max-size)
    (let ((result (create-random-from-production language '(start) max-size nil)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

(defmethod dispatch-message-name ((message-name (eql 'message-mutate-lisp-cfg-language)) message administrator stream)
  (let* ((functions (first (content message)))
         (constants (system-get (second (content message))))
         (variables (third (content message)))
         (max-size (fourth (content message)))
         (object (fifth (content message)))
         (operator (system-get 'mutate-cfg))
         (language (language (create-lisp-cfg-language
                    (or functions (entity-function-default-functions-info))
                    variables 
                    constants))))
    (setf (max-size language) max-size)
    (let ((result (mutate-cfg object language operator)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

(defmethod dispatch-message-name ((message-name (eql 'message-crossover-lisp-cfg-language)) message administrator stream)
  (let* ((functions (first (content message)))
         (constants (system-get (second (content message))))
         (variables (third (content message)))
         (max-size (fourth (content message)))
         (object-a (fifth (content message)))
         (object-b (sixth (content message)))
         (language (language (create-lisp-cfg-language
                    (or functions (entity-function-default-functions-info))
                    variables 
                    constants))))
    (setf (max-size language) max-size)
    (let ((result (directed-crossover-cfg object-a object-b language operator)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))