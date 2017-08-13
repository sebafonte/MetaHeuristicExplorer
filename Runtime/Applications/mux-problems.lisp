
;; MUX Language definition 
(defparameter *mux-expression-tokens*
  '((int-not :1-ary-operator)
    (int-and :2-ary-operator)
    (int-or  :2-ary-operator)
    (int-ife :3-ary-operator)))

(defun initialize-mux-expression-parser (name)
  (eval
   `(defparser ,name
               ((start expresion)
                $1)
               ((expresion :open :1-ary-operator expresion :close)
                `(:expresion (,$2 ,$3)))
               ((expresion :open :2-ary-operator expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4)))
               ((expresion :open :3-ary-operator expresion expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5)))
               ((expresion :var)
                `(:expresion ,$1)))))

(defun mux-grammar-productions ()
  '((start expresion)
    (expresion :open 1-ary-operator expresion :close)
    (expresion :open 2-ary-operator expresion expresion :close)
    (expresion :open 3-ary-operator expresion expresion expresion :close)
    (expresion var)
    (var :var)))


(defun entity-mux-default-functions-info ()
  '((int-and 2) (int-or 2) (int-not 1) (int-ife 3)))


(defvar *mux-editing-patterns*
  '(((int-not (int-not ?exp))   ?1)
    ((int-and ?exp ?exp)        ?1)
    ((int-or  ?exp ?exp)        ?1)))
   ;((ife ?a (ife ?a ?b ?c) ?d) (ife ?a ?b ?d))
   ;((ife ?a (and ?a ?b) ?c)    (ife ?a ?b ?c))
   ;((ife ?a ?a ?a)             ?a)



(defun int-and (a b)
  (declare (fixnum a) (fixnum b))
  ;; #TODO #OPTIMIZATION: Use #'logtest if possible
  (if (and (= a 1) (= b 1))
      1
    0))

(defun int-or (a b)
  ;; #TODO #OPTIMIZATION: Use #'logtest if possible
  (declare (fixnum a) (fixnum b))
  (if (or (= a 1) (= b 1))
      1
    0))

(defun int-not (a)
  (declare (fixnum a))
  (if (= a 1)
      0
    1))

(defun int-ife (a b c)
  (declare (fixnum a) (fixnum b) (fixnum c))
  (if (= a 1)
      b
    c))

;;; MUX Object problem definition
(defclass entity-mux (entity-function)
  ())


(defmethod compiled-program-6-mux ((o entity-mux))
  "Answer the compiled function that representing o genotype."
  (let ((compiler::*compiler-warnings* nil))
    (compile nil `(lambda () 
                    (declare (special a0 a1 d0 d1 d2 d3))
                    ,(program o)))))

(defmethod compiled-program-11-mux ((o entity-mux))
  "Answer the compiled function that representing o genotype."
  (let ((compiler::*compiler-warnings* nil))
    (compile nil `(lambda () 
                    (declare (special a0 a1 a2 d0 d1 d2 d3 d4 d5 d6 d7))
                    ,(program o)))))

;;; Environment auxiliars
(defmethod default-language ((o entity-mux))
  (system-get '6-default-mux-language))

(defmethod possible-languages ((o entity-mux))
  (list 
   (system-get '6-default-mux-language)
   (system-get '11-default-mux-language)))

(defmethod default-fitness-evaluators ((object entity-mux))
  "Answer the default classes that can evaluate object fitness."
  (list 
   (system-get '6-mux-default-evaluator)
   (system-get '11-mux-default-evaluator)))

(defmethod drawablep ((o entity-mux))
  "Answer whether <o> can be displayed on the GUI."
  t)


;;; Fitness evaluator
(defclass mux-fitness-evaluator (entity-evaluator)
  ((fitness-function :initarg :fitness-function :accessor fitness-function)))


(defmethod evaluate ((evaluator mux-fitness-evaluator) (object entity-mux))
  "Use <evaluator> to calculate and answer <object> fitness."
  (funcall (fitness-function evaluator) evaluator object))

(defmethod objective-class ((evaluator mux-fitness-evaluator))
  'entity-mux)

(defun bit-value (integer position)
  (declare (fixnum integer) (fixnum position))
  (if (> (logand integer (expt 2 position)) 0)
      1 
    0))

(defun evaluate-6-mux (evaluator object)
  "Evaluation method for 6-mux problem."
  (let ((matches 0)
        (compiled-program (compiled-program-6-mux object))
        (array (make-array (list 4))))
    (dotimes (i (expt 2 6))
      (let ((a0 (bit-value i 0))
            (a1 (bit-value i 1))
            (d0 (bit-value i 2))
            (d1 (bit-value i 3))
            (d2 (bit-value i 4))
            (d3 (bit-value i 5)))
        (declare (special a0) (special a1) (special d0) (special d1) (special d2) (special d3))
        (setf (aref array 0) d0
              (aref array 1) d1
              (aref array 2) d2
              (aref array 3) d3)
        (let ((case-value (aref array (+ a0 (* a1 2)))))
          (when (= (funcall compiled-program) case-value)
            (incf matches)))))
    (setf (fitness object) matches)
    matches))

(defmethod evaluate-11-mux (evaluator object)
  "Evaluation method for 11-mux problem."
  (let ((matches 0)
        (compiled-program (compiled-program-11-mux object))
        (array (make-array (list 8))))
    (dotimes (i (expt 2 11))
      (let ((a0 (bit-value i 0))
            (a1 (bit-value i 1))
            (a2 (bit-value i 2))
            (d0 (bit-value i 3))
            (d1 (bit-value i 4))
            (d2 (bit-value i 5))
            (d3 (bit-value i 6))
            (d4 (bit-value i 7))
            (d5 (bit-value i 8))
            (d6 (bit-value i 9))
            (d7 (bit-value i 10)))
        (declare (special a0) (special a1) (special a2) (special d0) (special d1) (special d2) (special d3)
                 (special d4) (special d5) (special d6) (special d7))
        (setf (aref array 0) d0
              (aref array 1) d1
              (aref array 2) d2
              (aref array 3) d3
              (aref array 4) d4
              (aref array 5) d5
              (aref array 6) d6
              (aref array 7) d7)
        (let ((case-value (aref array (+ a0 (* a1 2) (* a2 4)))))
          (when (= (funcall compiled-program) case-value)
            (incf matches)))))
    (setf (fitness object) matches)
    matches))



;;; Add system objects
(system-add
 ;; Grammars for mux problems
 (make-instance 'context-free-grammar
                :name 'default-mux-grammar
                :lexer 'lisp-math-expression-lexer
                :parser-initializer 'initialize-mux-expression-parser
                :productions (mux-grammar-productions)
                :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :expresion)))

(system-add
 ;; languages for mux problems
 (make-instance 'cfg-tree-language 
                :name '6-default-mux-language
				:description "6 mux default"
                :grammar (system-get-copy 'default-mux-grammar)
                :simplification-patterns *mux-editing-patterns*
                :functions (entity-mux-default-functions-info)
                :terminals '(d0 d1 d2 d3 a0 a1)
                :variables '(d0 d1 d2 d3 a0 a1)
                :tokens *mux-expression-tokens*
                :valid-new-expresion-function 'create-new-random-valid
                :simplification-function 'simplify-strategy
                :operators (default-genetic-operators-probability-lisp-expression))
 (make-instance 'cfg-tree-language 
                :name '11-default-mux-language
                :description "11 mux default"
                :grammar (system-get-copy 'default-mux-grammar)
                :simplification-patterns *mux-editing-patterns*
                :functions (entity-mux-default-functions-info)
                :terminals '(d0 d1 d2 d3 d4 d5 d6 d7 a0 a1 a2)
                :variables '(d0 d1 d2 d3 d4 d5 d6 d7 a0 a1 a2)
                :tokens *mux-expression-tokens*
                :valid-new-expresion-function 'create-new-random-valid
                :simplification-function 'simplify-strategy
                :operators (default-genetic-operators-probability-lisp-expression))
  ;; Mux problems fitness evaluators
  (make-instance 'mux-fitness-evaluator
                 :name '6-mux-default-evaluator
                 :description "6 mux default evaluator"
                 :fitness-function 'evaluate-6-mux
                 :min-fitness 0
                 :max-fitness 64
                 :solution-fitness 64)
  (make-instance 'mux-fitness-evaluator
                 :name '11-mux-default-evaluator
                 :description "11 mux default evaluator"
                 :fitness-function 'evaluate-11-mux
                 :min-fitness 0
                 :max-fitness 2048
                 :solution-fitness 2048))

