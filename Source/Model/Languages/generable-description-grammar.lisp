
(defclass generable-description-grammar (context-free-grammar)
  ((subject :initarg :subject :initform nil :accessor subject)))


(defparameter *description-grammar-tokens* nil)


;; #TODO: Move to language object ('generable-description-language)
(defun initialize-object-description-grammar-parser (name)
  (description-grammar-parser 
   name
   #|
   :algorithm-types '(generational-algorithm 
                      steady-state-algorithm)
   :language-types '(search-task 
                     entity-function-x-y 
                     population-generator 
                     fitness-evaluator)
   :genetic-operator-types '(search-task 
                             entity-function-x-y
                             population-generator
                             fitness-evaluator)
   :evaluator-types '(search-task 
                      entity-function-x-y 
                      population-generator 
                      fitness-evaluator)
    |#))

#|
;; Genetic operator language description (3 types)
((language-functions-description-object :language-functions-object)
 `(,$1))
((language-functions-description-search-task :language-functions-search-task)
 `(,$1))
((language-functions-description-operator :language-functions-operator)
 `(,$1))


(defun generate-description-grammar (node parent)
  (dolist (i (evolvable-properties node))
    nil))
|#

(setf gg (make-instance 'generable-description-grammar
                        :subject (make-instance 'search-task)
                        :name 'search-task-sample-grammar
                        :lexer 'object-description-grammar-lexer
                        :parser-initializer 'initialize-object-description-grammar-parser))
