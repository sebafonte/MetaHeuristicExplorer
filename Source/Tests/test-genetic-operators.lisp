(defclass test-genetic-operators (test-base-model) ())


(defmethod default-algorithm ((o test-genetic-operators) &optional &key (class 'generational-algorithm))
  "Answer default algorithm for <o>."
  (let* ((algorithm (make-instance class))
         (task (make-instance 'search-task :algorithm algorithm)))
    (setf (language task) (system-get 'lisp-math-function-xy))
    algorithm))

(defun prepare-size-in-algorithm (algorithm max-size)
  (declare (ignore max-size))
  (setf (max-size (language algorithm)) 10
        (max-depth (language algorithm)) 10))

(defmethod test-crossover-koza ((o test-genetic-operators))
 "Test crossover-koza operator with examples."
  (let ((algorithm (default-algorithm o))
        (operator (system-get 'crossover-koza)))
    (prepare-size-in-algorithm algorithm 10)
    (labels ((test (exp)
               (check (<= (tree-size exp) (max-size algorithm)))))
      ;; Create new functions with crossover #1 (expresion expresion)
      (dotimes (i 100)
        (multiple-value-bind (x)
            (operate operator algorithm (list '(cos (cos (sin (sin (sin y))))) '(1 2 3 4 5 6 7 8 9 10)))
          (test x)))
      ;; Create new functions with crossover #2 (expresion atom)
      (dotimes (i 100)
        (multiple-value-bind (x) 
            (operate operator algorithm (list '(cos (sin (sin (sin (sin y))))) 'x))
          (test x)))
      ;; Create new functions with crossover #3 (atom expresion)
      (dotimes (i 100)
        (multiple-value-bind (x y) 
            (operate operator algorithm (list 'x '(+ 1 2 3 4 5 6 7 8 9 10)))
          (test x)
          ;; Check if it's an atom, it should be the parent (strange but i should check Koza operator #CHECK)
          (if (= (tree-size x) 1) (check (equal x 'x)))
          (check (> (tree-size y) 1))))
      ;; Create new functions with crossover #4 (atom atom)
      (dotimes (i 100)
        (multiple-value-bind (x y) 
            (operate operator algorithm (list 'x 'y))
          (test x)
          ;; Check it's y
          (check (equal x 'x))
          (check (equal y 'y)))))))

(defmethod test-crossover ((o test-genetic-operators))
  "Test crossover operator with examples."
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'crossover))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 10)
    (labels ((test (exp)
               (check (<= (tree-size exp) (max-size algorithm)))))
      ;; Create new functions with crossover #1 (expresion expresion)
      (dotimes (i 100)
        (multiple-value-bind (x y)
            (operate operator language (list '(cos (cos (sin (sin (sin y))))) '(+ 1 2 3 4 5 6 7 8 9)))
          (test x)
          (test y)))
      ;; Create new functions with crossover #2 (expresion atom)
      (dotimes (i 100)
        (multiple-value-bind (x y) 
            (operate operator language (list '(cos (sin (sin (sin (sin y))))) 'x))
          (test x)
          (test y)
          ;; Test both expressions have source root
          (check (find-subtree x 'x))
          (check (find-subtree y 'y))))
      ;; Create new functions with crossover #3 (atom expresion)
      (dotimes (i 100)
        (multiple-value-bind (x y) 
            (operate operator language (list 'x '(+ 1 2 3 4 5 6 7 8 9)))
          (test x)
          (test y)
          (check (find-subtree y 'x))))
      ;; Create new functions with crossover #4 (atom atom)
      (dotimes (i 100)
        (multiple-value-bind (x y) 
            (operate operator language (list 'x 'y))
          (test x)
          (test y)
          ;; Test possible combinations
          (check (equal (car x) 'y))
          (check (equal (car y) 'x)))))))

(defmethod test-branch-delete ((o test-genetic-operators))
  "Test branch-delete genetic operation result execution and size."
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'branch-delete))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 10)
    (labels ((test-expresion (exp)
               (let ((new-exp (operate operator language (list exp))))
                 (check (<= (tree-size new-exp) (tree-size exp))))))
      ;; Test execution
      (dolist (exp '((x) (* x y) (* (sin (cos (cos x))) 3)))
        (test-expresion exp))
      ;; Create 100 programs (size from 1 to 10) and make 10 deletions on each one
      (dotimes (i 100)
        (let ((exp (create-expresion language 100 (mod i 10) t nil)))
          (dotimes (j 10)
            (test-expresion exp)))))))

(defmethod test-mutate ((o test-genetic-operators))
  "Test mutate genetic operation with examples."
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'mutate))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 10)
    ;; Random cases
    (dotimes (i 100)
      (let ((exp (create-expresion language 100 (mod i 10) t nil)))
        (dotimes (i 10)
          (let ((new-exp (operate operator language (list exp))))
            (check (<= (tree-size new-exp) (max-size algorithm)))))))
    ;; Verify to mutate a 17 node individual
    (dolist (case (list '(- (- X (/- X (/- (* X X) X))) (sin (/- (* X 3.0) 2.0)))
                        '(- (- X (/- X (/- (* X X) X))) (sin (/- (* X (cos 3.0)) 2.0)))
                        '(+ 1 2 (* 3 (cos 4) (sin (cos y))) (/ x y) 6 7)))
      (dotimes (i 100)
        (let ((new-exp (operate operator language (list case))))
          (check (<= (tree-size new-exp) (max-size algorithm))))))
    ;; Verify to mutate a 19 nodes individual where the only replacement can be done at root
    (dotimes (i 100)
      (let ((new-exp (operate operator language (list '(+ 1 2 3 4 10 11 12 13 14 15 16 17 18 19 20 21 22 23)))))
        (check (<= (tree-size new-exp) (max-size algorithm)))))))

(defmethod test-mutate-point  ((o test-genetic-operators))
  "Test 'mutate-point genetic operator with examples."
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'mutate-point))
         (language (language algorithm)))
    (prepare-size-in-algorithm algorithm 10)
    (labels ((test-expresion (exp)
               (let ((new-exp (operate operator language (list exp))))
                 (check (<= (tree-size new-exp) (tree-size exp))))))
      ;; Test some examples near max-size
      (dotimes (i 100)
        (test-expresion '(+ 1 2 3 4 10 11 12 13 14 15 16 17 18 19 20 21 22)))
      ;; Test some examples
      (dotimes (i 100)
        (test-expresion '(+ x y))))))

#|
;; #TODO
(defmethod test-replace-internal-subtree ((o test-genetic-operators))
  "Test #'replace-internal-subtree."
  nil)

(defmethod test-n-branch-delete ((o test-genetic-operators))
  "Test 'n-branch-delete genetic operator."
  nil)
    
(defmethod test-relacion-weight-nodes-weight-nodes-print ((o test-genetic-operators))
  "Test #'weight-nodes and #'weight-nodes-print."
  nil)
|#
