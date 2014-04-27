
(defclass test-constant-simplification (test-base-model) 
  ())


(defmethod test-constant-expressions ((o test-constant-simplification))
  "Verify that 'subexp-constant-p for random expresions can be executed."
  (let* ((algorithm (default-algorithm o))
         (language (language algorithm)))
    (dotimes (i 50) 
      (let ((exp (create-expresion language (max-size algorithm) (max-depth algorithm) t nil)))
        (format t "Exp: ~A~%Constant:~A~%~% " exp (subexp-constant-p exp language))))))

(defmethod test-constant-expressions-simplify-1 ((o test-constant-simplification))
  "Verify some expresion simplification cases."
  (let* ((algorithm (default-algorithm o))
         (language (language algorithm))
         (method (system-get 'optimize-lisp-math-simplification))
         (object (make-instance 'object-in-search
                                :object (make-instance 'entity-function-x-y)
                                :context (context algorithm))))
    (prepare-size-in-algorithm algorithm 10)
    (labels ((test (exp &optional exp-constant)
               (setf (program (object object)) exp)
               (execute-optimization-on method object)
               (let ((new-exp (program (object object))))
                 (check (<= (tree-size new-exp) (tree-size exp)))
                 (if exp-constant (check (equal new-exp exp-constant))))))
      ;; Test some basic cases
      (test '(+ 1 2) 3)
      (test '(+ x (+ 1 2)) '(+ x 3))
      (setf (program (object object)) '(+ 5 (sin (sin (sin (sin 0))))))
      (check (equal (coerce (execute-optimization-on method object) 'float) 5.0)))))

(defmethod test-constant-expressions-simplify-2 ((o test-constant-simplification))
  "Verify some expresion simplification cases."
  (let ((algorithm (default-algorithm o)))
    (check (equal (simplify-strategy nil '(+ (* x 0) (+ y (/- 0 (/- x 1)))) algorithm) 'y))
    (check (equal (simplify-strategy nil '(+ (+ 0 (- x x)) (+ x 0)) algorithm) 'x))
    ;; Not contempled cases
    (check (equal (simplify-strategy nil '(+ (/- (/- x 1) x) (/- x x)) algorithm) '2))
    (check (equal (simplify-strategy nil '(+ (/- (/- x 1) 1) (cos (/- x 1))) algorithm) '(+ x (cos x))))
    (check (equal (simplify-strategy nil '(+ (+ x 0) (+ 0 x) (+ (+ (+ x 0) 0) 0) (+ 0 (+ 0 (+ 0 x)))) algorithm) '(+ X X X X)))
    (check (equal (simplify-strategy nil '(+ (* x 0) (* 0 x) (* (* (* x 0) x) x) (* 7 (* x (* 0 x)))) algorithm) '(+ 0 0 0 0)))
    (check (equal (simplify-strategy nil '(+ (* x 1) (* 1 x) (* (* (* x 1) 1) 1) (* 1 (* 1 (* 1 x)))) algorithm) '(+ X X X X)))
    (check (equal (simplify-strategy nil '(+ (* x 1) (* 1 x) (* (* (* x 1) 1) 1) (* 1 (* 1 (* 1 x)))) algorithm) '(+ X X X X)))))

(defmethod test-constant-expressions-simplify-3 ((o test-constant-simplification))
  "Verify some expresion simplification cases."
  (let ((algorithm (default-algorithm o)))
    (equal (simplify-strategy nil '(+ (+ x 0.0) (+ 0.0 x) (+ x 0) (+ 0 x) (+ (+ (+ x 0) 0) 0) (+ 0 (+ 0 (+ 0 x)))) algorithm)
           '(+ x x x x x x))
    (equal (simplify-strategy nil '(+ (* x 0.0) (* 0.0 x) (* x 0) (* 0 x) (* (* (* x 0) x) x) (* 7 (* x (* 0 x)))) algorithm)
           '(+ 0 0 0 0 0 0))
    (equal (simplify-strategy nil '(+ (* x 1.0) (* x 1) (* 1 x) (* 1.0 x) (* (* (* x 1) 1) 1) (* 1 (* 1 (* 1 x)))) algorithm)
           '(+ x x x x x x))
    (equal (simplify-strategy nil '(+ (/- x 1) (/- x 1.0) (/- 0 x) (/- 0.0 x)) algorithm)
           '(+ x x 0 0))))
