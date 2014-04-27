(defclass test-grammar (test-base-model) ())


(defparameter *test-grammar-tokens*
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

(defun test-grammar-lexer ()
  (let ((symbol (pop *parser-input*)))
    (if symbol (test-grammar-get-token symbol)
      nil)))

(defun test-grammar-get-token (word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table *test-grammar-tokens* word)))
    (if (equal token-type :unknown) 
        (setf token-type 
              (if (numberp word) :constant)))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

;; Example 1

(defmethod default-test-language-1 ((o test-grammar))
  (make-instance 'cfg-tree-language 
                 :name 'test-sample-language-1
                 :grammar (default-test-grammar-1 o)
                 :tokens *test-grammar-tokens*))

(defun default-test-grammar-1-productions ()
  '((start lv)
    (lv lv :var)
    (lv :var)))

(defmethod default-test-grammar-1 ((o test-grammar))
  "Answer a test grammar for testing."
  (make-instance 'context-free-grammar
                 :name 'sample-test-grammar 
                 :lexer 'test-grammar-lexer
                 :productions (default-test-grammar-1-productions)
                 :parser-initializer (lambda (name) (eval `(defparser ,name ((start :var) $1))))))

(defmethod test-calculate-minimum-production-size-1 ((o test-grammar))
  "Verify calculate-minimum-production-size with an example."
  (let* ((grammar (grammar (default-test-language-1 o)))
         (table (minimum-production-sizes grammar)))
      (check 
        (= (gethash 'start table) 1)
        (= (gethash 'lv table) 1))))

;; Example 2

(defmethod default-test-language-2 ((o test-grammar))
  (make-instance 'cfg-tree-language 
                 :name 'test-sample-language-2
                 :grammar (default-test-grammar-2 o)
                 :tokens *test-grammar-tokens*))

(defun default-test-grammar-2-productions ()
  '((start object)
    (object :search-object)
    (object best-of-task)
    (best-of-task :open :best-of-task task-description :close)
    (task-description :task-description generator-description)
    (generator-description generator-object)
    (generator-object :open :generator-random-object :close)
    (generator-object :open :generator-bests-object task-description :close)))

(defmethod default-test-grammar-2 ((o test-grammar))
  "Answer a test grammar for testing."
  (make-instance 'context-free-grammar
                 :name 'sample-test-grammar 
                 :lexer 'test-grammar-lexer
                 :productions (default-test-grammar-2-productions)
                 :parser-initializer (lambda (name) (eval `(defparser ,name ((start :var) $1))))))

(defmethod test-calculate-minimum-production-size-2 ((o test-grammar))
  "Verify calculate-minimum-production-size with an example."
  (let* ((grammar (grammar (default-test-language-2 o)))
         (table (minimum-production-sizes grammar)))
    (check
      (= (gethash 'start table) 1)
      (= (gethash 'object table) 1)
      (= (gethash 'best-of-task table) 3)
      (= (gethash 'task-description table) 2)
      (= (gethash 'generator-description table) 1)
      (= (gethash 'generator-object table) 1))))

;; Example 3

(defmethod default-test-language-3 ((o test-grammar))
  (make-instance 'cfg-tree-language 
                 :name 'test-sample-language-3
                 :grammar (default-test-grammar-3 o)
                 :tokens *test-grammar-tokens*
                 :functions '((+ 2))))

(defun default-test-grammar-3-productions ()
  '((start expresion)
    (expresion :open 1-ary-operator expresion :close)
    (expresion :open 2-ary-operator expresion expresion :close)
    (expresion :open 3-ary-operator expresion expresion expresion :close)
    (expresion :constant)
    (expresion :var)))

(defmethod default-test-grammar-3 ((o test-grammar))
  "Answer a test grammar for testing."
  (make-instance 'context-free-grammar
                 :name 'sample-test-grammar 
                 :lexer 'test-grammar-lexer
                 :productions (default-test-grammar-3-productions)
                 :parser-initializer (lambda (name) (eval `(defparser ,name ((start :var) $1))))))

(defmethod test-calculate-minimum-production-size-3 ((o test-grammar))
  "Verify calculate-minimum-production-size with an example."
  (let* ((grammar (grammar (default-test-language-3 o)))
         (table (minimum-production-sizes grammar)))
      (check 
        (= (gethash 'start table) 1)
        (= (gethash 'expresion table) 1)
        (= (gethash '1-ary-operator table) *infinite-productions-size-value*)
        (= (gethash '2-ary-operator table) 1))))

