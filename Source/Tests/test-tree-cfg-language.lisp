(defclass test-tree-cfg-language (test-base-model) ())

(defparameter *test-tree-cfg-grammars-tokens*
  '(;; 1 argument operators
    (abs :1-ary-operator)
    (sin :1-ary-operator)
    (cos :1-ary-operator)
    (tan :1-ary-operator)
    (sqr :1-ary-operator)
    (exp :1-ary-operator)
    (log :1-ary-operator)
    ;; 2 argument operators
    (+ :2-ary-operator)
    (- :2-ary-operator)
    (* :2-ary-operator)
    (/ :2-ary-operator)
    ;; 3 argument operators
    (real-if :3-ary-operator)))


(defmethod default-tree-cfg-language ((o test-tree-cfg-language))
  (make-instance 'cfg-tree-language 
                 :name 'default-tree-cfg-language
                 :grammar (default-tree-cfg-grammar o)
                 :tokens *test-tree-cfg-grammars-tokens*
                 :functions '((+ 2) (abs 1))
                 :variables '(x y)))

(defun default-tree-cfg-language-productions ()
  '((start expresion)
    (expresion :open 1-ary-operator expresion :close)
    (expresion :open 2-ary-operator expresion expresion :close)
    (expresion :open 3-ary-operator expresion expresion expresion :close)
    (expresion :constant)
    (expresion var)
    (var :var)))

(defmethod default-tree-cfg-grammar ((o test-tree-cfg-language))
  "Answer a test grammar for testing."
  (make-instance 'context-free-grammar
                 :name 'sample-test-grammar 
                 :lexer 'test-grammar-lexer
                 :productions (default-tree-cfg-language-productions)
                 :parser-initializer (lambda (name) (eval `(defparser ,name ((start :var)))))))

(defmethod test-code-generation-size ((o test-tree-cfg-language))
  (let ((language (default-tree-cfg-language o)))
    (dotimes (i 6)
      (let ((max-size (1+ i)))
        (dotimes (j 25)
          (let ((exp (create-random-from-production
                      language '(start) max-size 'lambda-weight-equal-random-selection-list)))
            (check (<= (tree-size exp) max-size))))))))

(defmethod test-code-generation-functions ((o test-tree-cfg-language))
  (let ((language (default-tree-cfg-language o)))
    (dotimes (j 50)
      (let ((exp (create-random-from-production 
                  language '(start) 20 'lambda-weight-equal-random-selection-list)))
        (dolist (i (flatten exp))
          (check (or (numberp i)
                     (equal 'x i)
                     (equal 'y i)
                     (equal '+ i)
                     (equal 'abs i))))))))

