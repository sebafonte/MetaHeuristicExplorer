(defclass test-source-code-conversion (test-base-model) 
  ())


;;; AUXILIAR METHODS
(defmethod verify-source-code-persistence ((test test-source-code-conversion) (object t) &optional (function nil))
  (check
    (apply function (list (eval (new-source-description object)) object))))

(defmethod verify-source-code-persistence-eval ((test test-source-code-conversion) object)
  (eval (new-source-description object)))

(defmethod verify-source-code-persistence-eq ((test test-source-code-conversion) object)
  (verify-source-code-persistence test object #'eq))

(defmethod verify-source-code-persistence-equal ((test test-source-code-conversion) object)
  (verify-source-code-persistence test object #'equal))

(defmethod verify-source-code-persistence-equals ((test test-source-code-conversion) object)
  (verify-source-code-persistence test object #'equals))

(defmethod verify-source-code-persistence-equalp ((test test-source-code-conversion) object)
  (verify-source-code-persistence test object #'equalp))


;;; TEST METHODS
(defmethod test-basic-objects-equality ((o test-source-code-conversion))
  ;; CASES: Basic cases
  (verify-source-code-persistence-equal o 222)
  (verify-source-code-persistence-equal o "A string")
  (verify-source-code-persistence-equal o '(() () ((((())))) () ()))
  (verify-source-code-persistence-equalp o #(2 2))
  (verify-source-code-persistence-equalp o (make-array '(3) :initial-contents '(1 2 a)))
  (verify-source-code-persistence-equalp o (make-array '(2 3) :initial-contents '((a b c) (a c 2))))
  (verify-source-code-persistence-equal o '(a b c))
  (verify-source-code-persistence-equal o '((((((a)))))))
  (verify-source-code-persistence-equal o '(make-array 25 :initial-element 6))
  (verify-source-code-persistence-equalp o (make-array 25 :initial-element 6))
  (verify-source-code-persistence-equalp o (make-hash-table))
  (verify-source-code-persistence-equal o (find-class 'entity))
  (verify-source-code-persistence-equals o '(() () ((((())))) () ()))
  ;; CASE: Not empty hash table
  (let ((new-table (make-hash-table)))
    (setf (gethash 2 new-table) 4)
    (verify-source-code-persistence-equalp o new-table))
  ;; CASE: Not empty array
  (check 
    (equalp 
     (eval (new-source-description (make-array 25 :initial-element 6)))
     (make-array 25 :initial-element 6))))

(defmethod test-symbol-conversion ((o test-source-code-conversion))
  (verify-source-code-persistence-eq o 'a-symbol)
  (verify-source-code-persistence-eq o :a-symbol)
  (check
    (eq (eval (new-source-description #'-)) '-)
    (eq (eval (new-source-description #'class-name)) 'class-name)))

(defmethod test-business-objects-conversion-eval ((o test-source-code-conversion))
  (verify-source-code-persistence-eval o (make-instance 'log-data-container))
  (verify-source-code-persistence-eval o (make-instance 'property))
  (verify-source-code-persistence-eval o (make-instance 'population :count-individuals 20))
  (verify-source-code-persistence-eval o (make-instance 'entity))
  (verify-source-code-persistence-eval o (make-instance 'genotype))
  (verify-source-code-persistence-eval o (make-instance 'genotype :expresion '(+ x y (* x x) (* x x x) (* x x x x x))))
  (verify-source-code-persistence-eval o (make-instance 'entity-function-x-y))
  (verify-source-code-persistence-eval o (make-instance 'search-algorithm))
  (verify-source-code-persistence-eval o (make-instance 'evolutionary-algorithm))
  (verify-source-code-persistence-eval o (make-instance 'generational-algorithm))
  (verify-source-code-persistence-eval o (make-instance 'steady-state-algorithm))
  (verify-source-code-persistence-eval o (make-instance 'manual-search-algorithm))
  (verify-source-code-persistence-eval o (make-instance 'search-task)))

(defmethod test-business-objects-equality ((o test-source-code-conversion))
  ;; CASE: Vector equals
  (verify-source-code-persistence-equals o (make-instance 'image-vector-3d :x 10 :y 20 :z 30))
  ;; CASE: Program equal 'entity-function-x-y
  (let ((object (make-instance 'entity-function-x-y)))
    (setf (program object) '(sin x))
    (equal (program (eval (new-source-description object)))
           (program object)))
  ;; CASE: Program equal 'entity-linear-ordering-list
  (let ((object (make-instance 'entity-linear-ordering-list :matrix #(2 2))))
    (equalp (program (eval (new-source-description object)))
            (program object))))
  
(defmethod test-same-object-equality ((o test-source-code-conversion))
  ;; CASE: List with various instances of the same object
  (let* ((object (make-instance 'entity))
         (new-object (eval (new-source-description (list object object object)))))
    (check 
      (equals (program object) (program (first new-object)))
      (equals (program object) (program (second new-object)))
      (equals (program object) (program (third new-object)))
      (eq (first new-object) (second new-object))
      (eq (second new-object) (third new-object))))
  ;; CASE: Arrays with example object
  (let* ((object (make-array 2 :initial-element (make-instance 'entity)))
         (new-object (eval (new-source-description object))))
    (check (eq (aref new-object 0) (aref new-object 1)))))

(defmethod test-system-objects-equality ((o test-source-code-conversion))
  ;; Genetic operators
  (verify-source-code-persistence-eq o (system-get 'crossover))
  (verify-source-code-persistence-eq o (system-get 'crossover-cfg))
  (verify-source-code-persistence-eq o (system-get 'crossover-koza))
  (verify-source-code-persistence-eq o (system-get 'mutate))
  (verify-source-code-persistence-eq o (system-get 'mutate-point))
  (verify-source-code-persistence-eq o (system-get 'mutate-koza))
  (verify-source-code-persistence-eq o (system-get 'branch-delete))
  (verify-source-code-persistence-eq o (system-get 'permutate-random-row))
  (verify-source-code-persistence-eq o (system-get 'permutate-random-column))
  ;; Evaluators
  (verify-source-code-persistence-eq o (system-get 'medium-0-10-function-x-y-evaluator))
  ;; Population initializers
  (verify-source-code-persistence-eq o (system-get 'random-trees-initializer))
  (verify-source-code-persistence-eq o (system-get 'fixed-expressions-initializer))
  (verify-source-code-persistence-eq o (system-get 'ramped-half-and-half-initializer))
  (verify-source-code-persistence-eq o (system-get 'sample-lop-initializer))
  ;; Grammars
  (verify-source-code-persistence-eq o (system-get 'lisp-math-function-grammar-x-y)))

(defmethod test-array-dimensions-cases ((o test-source-code-conversion))
  (let ((cases (list 
                #(1 2 3 4)
                #1A(1 2 3 4 5 6 7)
                #2A((1 2 3) (4 5 6))
                #3A(((1 2) (3 4)) ((6 7) (8 9)))
                #4A((((1 2) (3 4)) ((5 6) (7 8))) (((9 0) (1 2)) ((3 4) (5 6)))))))
    (dolist (array cases)
      (check (equals array (eval (source-description array)))))))

(defmethod test-long-array-case ((o test-source-code-conversion))
  (let ((case))
    ;; Fill case list
    (dotimes (i 10000)
      (appendf case (list i)))
    (setf case (make-array (length case) :initial-contents case))
    ;; Verify behaviour
    (check
      (handler-case
          (equals case (eval (source-description case)))
        (condition (c) nil)))))

(defmethod test-long-list-case ((o test-source-code-conversion))
  (let ((case))
    ;; Fill case list
    (dotimes (i 10000)
      (appendf case (list i)))
    ;; Verify behaviour
    (equals case (eval (source-description case)))))
