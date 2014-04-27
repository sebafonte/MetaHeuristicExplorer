(defclass test-constant-optimization (test-base-model) 
  ())


(defmethod test-constant-optimization-steepest-descent ((o test-constant-optimization))
  "Verify steepest descent optimization method behaves correctly for some few cases."
  (let* ((algorithm (default-algorithm o))
         (method (system-get 'optimization-method-steepest-descent))
         (object (make-instance 'object-in-search
                                :object (make-instance 'entity-function-x-y)
                                :context (context algorithm)))
         (evaluator (fitness-evaluator algorithm)))
    (labels ((test (original found)
               (setf (target-program evaluator) original
                     (samples evaluator) 10
                     (measure-start evaluator) 0
                     (measure-end evaluator) 10
                     (program (object object)) found)
               (initialize-fitness-data evaluator)
               (evaluate evaluator (object object))
               (execute-optimization-on method object)
               (check (points-equal-for-test (program (object object)) original))))
      (test '(+ x 1)           '(+ x 10))
      (test '(+ x 1)           '(+ x 5000))
      (test '(+ (+ x 2) 10)    '(+ (+ x 5) 20))
      (test '(+ x 7)           '(+ x 1))
      (test '(+ x 7000)        '(+ x 1))
      (test '(+ x (* x x) 1)   '(+ x (* x x) 100))
      (test '(* x 3)           '(* x 10))
      (test '(* x 10)          '(* x 3)))))

(defmethod test-normalize-gradient ((o test-constant-optimization))
  "Verify #'normalize-gradient behaves correctly with a few examples."
  (labels ((test (x y) 
             (check (equals (to-list (normalize-gradient x))
                            (to-list y)))))
    ;; Cases 1
    (test #(1) #(1.0))
    (test #(21) #(1.0))
    (test #(0.51) #(1.0))
    (test #(7 0) #(1.0 0.0))
    (test #(0 7) #(0.0 1.0))
    (test #(0 3 0) #(0.0 1.0 0.0))
    ;; Cases 2
    (labels ((test (x)
               (check (< (abs (- (apply '+ (mapcar (lambda (x) (* x x)) 
                                                   (to-list (normalize-gradient x))))
                                 1))
                         0.001))))
      (test #(1.5))
      (test #(1.5 1.5))
      (test #(1.5 1.5))
      (test #(1.5 1.5 1.5 1.5)))))

(defmethod test-get-constants-information ((object test-constant-optimization))
  "Verify #'get-constants-information works correctly.  "
  (let ((o (make-instance 'entity-function-x-y))
        (vars))
    ;; Case
    (setf (program o) '77
          vars (get-constants-information o))
    (check
      (find-if (lambda (x) (= (car x) 77)) vars))
    ;; Case
    (setf (program o) '(+ x (* 77 (- y (+ 99 y))))
          vars (get-constants-information o))
    (check
      (find-if (lambda (x) (= (car x) 77)) vars)
      (find-if (lambda (x) (= (car x) 99)) vars))
    ;; Case
    (setf (program o) '(+ 12 (* 77 (- y (+ 99 y))))
          vars (get-constants-information o))
    (check
      (find-if (lambda (x) (= (car x) 12)) vars)
      (find-if (lambda (x) (= (car x) 77)) vars)
      (find-if (lambda (x) (= (car x) 99)) vars))
    ;; Case
    (setf (program o) '(+ 12 (* 77 (- 33 (+ 99 y))))
          vars (get-constants-information o))
    (check
      (find-if (lambda (x) (= (car x) 12)) vars)
      (find-if (lambda (x) (= (car x) 77)) vars)
      (find-if (lambda (x) (= (car x) 33)) vars)
      (find-if (lambda (x) (= (car x) 99)) vars))))

(defun points-equal-for-test (expression-a expression-b)
  "Checks equality between the evaluation of <expression-a> and <expression-b>."
  (block 1
    (dotimes (i 20)
      (dotimes (j 20)
        (declare (special x) (special y))
        (setf x i y j)
        (when (>= (abs (- (eval expression-a) (eval expression-b))) 0.01)
          (return-from 1 nil))))
    t)) 
