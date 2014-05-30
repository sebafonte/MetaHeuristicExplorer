(defclass test-genetic-operators-cfg (test-base-model) ())


(defmethod default-algorithm-1 ((o test-genetic-operators-cfg) &optional &key (class 'generational-algorithm))
  "Answer default algorithm for <o>."
  (let* ((algorithm (make-instance class))
         (subtask (make-instance 'search-task :algorithm algorithm)))
    (setf (language subtask) (system-get 'lisp-math-function-xy))
    algorithm))

(defmethod default-algorithm-2 ((o test-genetic-operators-cfg) &optional &key (class 'generational-algorithm))
  "Answer default algorithm for <o>."
  (let* ((algorithm (make-instance class))
         (task (make-instance 'search-task :algorithm algorithm)))
    (setf (language task) (system-get 'polynomial-xy)
		  (max-size (language task)) 18)
    algorithm))

(defmethod test-crossover-cfg-execution ((o test-genetic-operators-cfg))
  "Verifies some basic examples for crossover-cfg operations work properly.
   #NOTE: By the moment, the only thing we verify is to dont get a runtime error."
  (let ((algorithm (default-algorithm o))
        (operator (system-get 'crossover-cfg)))
    (labels ((test (a b)
               (dotimes (i 100)
                 (check (not (null (operate operator algorithm (list a b))))))))
      (test 'x '3)
      (test '4 'y)  
      (test '3 '4)
      (test 'x 'y)
      (test 'x '(cos y))
      (test '3 '(cos y))
      (test '3 '(+ (* 3 (cos y)) y))
      (test '(cos x) '(sin x))
      (test '(cos x) '(sin y))
      (test '(+ 1 2) '(+ x y))
      (test '(+ 1 2) '(* x y))
      (setf (max-size (language (context algorithm))) 8)
      (test '(+ 1 2) '(+ (* 1 2) (* x y)))
      (test '(+ (* 1 2) (* x y)) '(+ 1 2)) 
      (test '(+ (* 1 2) (* x y)) '(sin 2))
      (test '(+ (* 1 2) (* x y)) '(sin (cos x)))
      (test '(+ (* x 4) (* 5 y)) '(+ (* 1 2) (* x y)))
      (test '(cos (sin (cos x))) '(sin (cos (sin (cos y)))))
      (test '(cos (sin (cos x))) '(sin (cos (sin (cos 3))))))))

(defmethod test-crossover-cfg-max-size-simple-lisp-math-expresion ((o test-genetic-operators-cfg))
  "Verifies crossover-cfg genetic operation dont exceed defined max-size. 
   #NOTE: It should not be neccesary to have two expression satisfing this constraint to produce
          children that satisfy it for this kind of language (simple-lisp-math-expresion)."
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'crossover-cfg))
         (language (language algorithm)))
    (labels ((test (a b)
               (dotimes (i 20)
                 (check (<= (tree-size (operate operator algorithm (list a b))) 
                            (max-size language))))))
      (setf (max-size language) 6)
      (test '(sin (cos (abs (* x y)))) 'x)
      (test '(sin (cos (abs (* x y)))) '(cos x))
      (test '(sin (cos (abs (* x y)))) '(cos (sin x))))))
      
(defmethod test-crossover-cfg-max-size-polynom-x-y ((o test-genetic-operators-cfg))
  "Verifies crossover-cfg genetic operation dont exceed defined max-size. 
   #NOTE: It should not be neccesary to have two expression satisfing this constraint to produce
          children that satisfy it for this kind of language (simple-lisp-math-expresion)."
  (let ((algorithm (default-algorithm o))
        (operator (system-get 'crossover-cfg))
        (language (system-get 'polynomial-xy)))
    (labels ((test (a b)
               (dotimes (i 10)
                 (let ((new-individual (operate operator algorithm (list a b))))
                   (check 
                     (<= (tree-size new-individual) (max-size language))
                     (<= (tree-depth new-individual) (max-depth language)))
                   (parse (grammar language) new-individual)))))
      (dotimes (max-size 10)
        (setf (language (context algorithm)) language
              (max-size (language (context algorithm))) (+ 14 max-size))
        (test '1 '5)
        (test '1 '(+ 1 (+ 3 (* y y))))
        (test '(+ (+ 3 (* 5 x) (* 7 x)) (+ (* 2.5 x) 2)) '(+ 3 (* 4 y) (* 2 x)))))))

(defmethod test-common-tokens-simple-lisp-math-expresion ((o test-genetic-operators-cfg))
  "Verifies whether #'common-tokens return correct values for some specific examples."
  (let ((algorithm (default-algorithm o)))
    (labels ((test (a b)
               (check 
                 (= (tree-size a) (tree-size b))
                 (equal (set-difference a b) nil))))
      ;; #NOTE: '(:var) is not a result because it doesnt belong to #'crossover-tokens
      (test (common-tokens algorithm '(:var) '(:var))           
            nil)
      (test (common-tokens algorithm '(:constant) '(:var))      
            nil)
      (test (common-tokens algorithm '(:var) '(:constant)) 
            nil)
      ;; #NOTE: '(:var) is not a result because it doesnt belong to #'crossover-tokens
      (test (common-tokens algorithm '(:constant) '(:constant)) 
            nil)
      (test (common-tokens algorithm '(:1-ary-operator) '(:var))
            nil)
      (test (common-tokens algorithm '(:1-ary-operator) '(:constant))
            nil)
      (test (common-tokens algorithm '(:1-ary-operator) '(:1-ary-operator))
            '(:1-ary-operator))
      (test (common-tokens algorithm '(:1-ary-operator) '(:1-ary-operator :2-ary-operator))           
            '(:1-ary-operator))
      (test (common-tokens algorithm '(:expresion :2-ary-operator) '(:expresion :1-ary-operator)) 
            '(:expresion))
      (test (common-tokens algorithm 
                           '(:1-ary-operator :var :expresion) 
                           '(:1-ary-operator :2-ary-operator :var :expresion)) 
            '(:1-ary-operator :expresion)))))

(defmethod test-select-indexes-execution-lisp-math-expresion ((o test-genetic-operators-cfg))
  "Verifies whether #'select-indexes-function return correct values for some specific examples."
  (let* ((algorithm (default-algorithm o))
         (grammar (grammar algorithm))
         (operator (system-get 'crossover-cfg)))
    (labels ((test (a b)
               (multiple-value-bind (x y)
                   (select-indexes-cfg (parse grammar a) (parse grammar b) algorithm operator)
                 (check (not (null (and x y)))))))
      (test 1 2)
      (test 1 'x)
      (test 'x 1)
      (test 'x 'y)
      (test 1 '(abs (cos (* x y))))
      (test '(abs (cos (* x y))) 1)
      (test '(abs (cos x)) '(cos (abs 3)))
      (test '(abs x) '(* x y)))))

(defmethod test-select-indexes-lisp-math-expresion ((o test-genetic-operators-cfg))
  (let* ((algorithm (default-algorithm o))
         (exp (parse (grammar algorithm) '(+ 1 2))))
    (labels ((test (a index b)
               (check (equal (select-subtree-cfg a index) b))))
      (test exp 7 nil)
      (test exp 1 '(:EXPRESION ((:2-ARY-OPERATOR +) (:EXPRESION (:CONSTANT 1)) (:EXPRESION (:CONSTANT 2)))))
      (test exp 2 '(:2-ARY-OPERATOR +))
      (test exp 3 '(:EXPRESION (:CONSTANT 1)))
      (test exp 4 '(:CONSTANT 1))
      (test exp 5 '(:EXPRESION (:CONSTANT 2)))
      (test exp 6 '(:CONSTANT 2))
      (test exp 7 nil))))
 
(defmethod test-directed-crossover-cfg-size ((o test-genetic-operators-cfg))
  "Verify a special case in 'directed-crossover-cfg."
  (let ((algorithm (default-algorithm-2 o)))
    (dotimes (i 100)
      (directed-crossover-cfg 
       '(+ (* 0 (* y y y y)))
       '(+ x x x x x x x x x x x x (* 0 (* x x)))
       algorithm
       (system-get 'crossover-cfg)))))

(defmethod test-branch-delete-cfg-lisp-math ((o test-genetic-operators-cfg))
  "Verify a special case in 'branch-delete-cfg."
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'branch-delete-cfg))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 10)
    (labels ((test-expresion (exp)
               (let ((new-exp (operate operator language (list exp))))
                 (check (<= (tree-size new-exp) (tree-size exp))))))
      ;; Test execution
      (dolist (exp '(x (* x y) (* (sin (cos (cos x))) 3)))
        (test-expresion exp))
      ;; Create 100 expressions (with size from 1 to 10) and execute 10 deletions
      (dotimes (i 100)
        (let ((exp (create-expresion language 100 (mod i 10) t nil)))
          (dotimes (j 10)
            (test-expresion exp)))))))

(defmethod test-mutate-production-cfg-lisp-math ((o test-genetic-operators-cfg))
  "Verify a special case in 'mutate-production-cfg."
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'mutate-production-cfg))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 10)
    (labels ((test-expresion (exp)
               (let ((new-exp (operate operator language (list exp))))
                 (check (<= (tree-size new-exp) (tree-size exp))))))
      ;; Test it works for some genetic operation
      (dolist (exp '(x (* x y) (* (sin (cos (cos x))) 3)))
        (test-expresion exp))
      ;; Create 100 expressions (with size from 1 to 10) and execute 10 deletions
      (dotimes (i 100)
        (let ((exp (create-expresion language 100 (mod i 10) t nil)))
          (dotimes (j 10)
            (test-expresion exp)))))))

(defmethod test-branch-delete-cfg-lisp-math-forced-delete ((o test-genetic-operators-cfg))
  "Test 'branch-delete operator with examples."
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'branch-delete-cfg))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 10)
    (labels ((test-expresion (exp)
               (let ((new-exp (operate operator language (list exp))))
                 (check 
                   (or (< (tree-size new-exp) (tree-size exp))
                       (= (tree-size exp) 1))))))
      ;; Test on known simplificable expressions
      (dolist (exp '(3
                     x
                     (* x y) 
                     (* (sin (cos (cos x))) 3) 
                     (+ (* x 2) y)
                     (+ (* x 2) (abs y))))
        (dotimes (i 10)
          (test-expresion exp))))))

(defmethod test-random-creation-lisp-math-size ((o test-genetic-operators-cfg))
  "Verify random-creation operation max size with sample cases."
   (let* ((algorithm (default-algorithm o))
          (operator (system-get 'random-create-cfg))
          (language (language algorithm)))
     (dotimes (i 100)
       (check (<= (tree-size (operate operator language (list nil)))
                  (max-size algorithm))))))

(defmethod test-branch-delete-cfg-polynomial ((o test-genetic-operators-cfg))
  "Test 'branch-delete operator with examples."
  (let* ((algorithm (default-algorithm-2 o))
         (operator (system-get 'branch-delete-cfg))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 20)
    (labels ((test-expresion (exp)
               (let ((new-exp (operate operator language (list exp exp))))
                 (check (< (tree-size new-exp) (tree-size exp))))))
      ;; Test it works for some genetic operation
      (dolist (exp '((+ x y)
                     (+ 1 y)
                     (+ 1 2)
                     (+ (* 1 (* y)))
                     (+ x (* 1 (* y)))
                     (+ y (* 1 (* y)))
                     (+ (* 1 (* y)) (* 2 (* x y)))))
        (dotimes (i 10)
          (test-expresion exp))))))

(defmethod test-mutate-production-cfg-polynomial ((o test-genetic-operators-cfg))
  "Test 'mutate-production-cfg operator with examples."
  (let* ((algorithm (default-algorithm-2 o))
         (operator (system-get 'mutate-production-cfg))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 20)
    (labels ((test-expresion (exp)
               (let ((new-exp (operate operator language (list exp exp))))
                 (check (<= (tree-size new-exp) (tree-size exp))))))
      ;; Test it works for some genetic operation
      (dolist (exp '((+ x y)
                     (+ 1 y)
                     (+ 1 2)
                     (+ (* 1 (* y)))
                     (+ x (* 1 (* y)))
                     (+ y (* 1 (* y)))
                     (+ (* 1 (* y)) (* 2 (* x y)))))
        (dotimes (i 10)
          (test-expresion exp))))))
