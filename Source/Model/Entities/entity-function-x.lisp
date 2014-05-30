
(defclass entity-function-x (entity-function)
  ())


(defmethod compiled-program ((o entity-function-x))
  "Answer the compiled function for <o>."
  (let ((compiler::*compiler-warnings* nil))
    (compile nil `(lambda () 
                    (declare (special x))
                    ,(program o)))))

(defmethod possible-languages ((o entity-function-x))
  (list 
   (system-get 'lisp-math-function-x)
   (system-get 'polynomial-x)
   (system-get 'compression-lisp-math-function-x)
   (system-get 'adf-lisp-math-function-x)))

(defmethod default-fitness-evaluators ((o entity-function-x))
  "Answer the default classes that can evaluate <o> fitness."
  (list 
   (system-get 'fine-0-10-function-x-evaluator)
   (system-get 'medium-0-10-function-x-evaluator)
   (system-get 'gross-0-10-function-x-evaluator)
   (system-get 'fine-0-10-function-x-cl-evaluator)
   (system-get 'medium-0-10-function-x-cl-evaluator)
   (system-get 'gross-0-10-function-x-cl-evaluator)
   (system-get 'sample-values-function-x-evaluator)
   (system-get 'sucession-evaluator)))

(defmethod drawablep ((o entity-function-x))
  "Answer whether <o> can be displayed on the GUI."
  t)