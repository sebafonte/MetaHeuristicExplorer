(defclass test-core-functions (test-base-model) ())


(defmethod test-get-random-element ((o test-core-functions))
  "Check #'get-random-element basic behavior with examples."
  (labels ((test (x y) 
             (let ((value (/ 1 (length y))))
               (find (get-random-element  
                      (mapcar (lambda (each) (substitute value 'v each)) x))
                     y
                     :test #'equal))))
    (check 
      ;; 1 element: normalized, non nnormalized and 0
      (test '((x 1))               '(x))
      ;; 2 elements: normalized sum, one element normalized for each one, idem but not normalized and zero
      (test '((x v) (y v))         '(x y))
      (test '((x v) (y 0))         '(x))
      (test '((x 0) (y v))         '(y))
      ;; 3 elements: idem 1 and 2 but we does not test each variable
      (test '((x v) (y v) (z v))   '(x y z))
      (test '((x 0) (y v) (z v))   '(y z))
      (test '((x v) (y 0) (z v))   '(x z))
      (test '((x v) (y v) (z 0))   '(x y))
      (test '((x 1) (y 0) (z 0))   '(x))
      (test '((x 0) (y 1) (z 0))   '(y))
      (test '((x 0) (y 0) (z 1))   '(z)))))

(defmethod test-tree-size ((o test-core-functions))
  "Check #'tree-size basic behavior with examples."
  (check (= (tree-size '1) 1)
         (= (tree-size '(1)) 1)
         (= (tree-size '(1 1)) 2)
         (= (tree-size '(+ 1 1)) 3)
         (= (tree-size '(+ 1 1 1)) 4)
         (= (tree-size '(+ (- x 1) 1)) 5)
         (= (tree-size '(+ (- x 1 y) 1 z)) 7)
         (= (tree-size '(+ (- x (sin (sin 1)) y) 1 z)) 9)
         (= (tree-size '(some-operator some-value some-value some-value)) 4)))

(defmethod test-tree-size-special-case ((o test-core-functions))
  "Check #'tree-size basic behavior with examples.
   #NOTE: Strage, just notifying actual behavior."
  (check 
    (= (tree-size '(+ 1 (2))) 3)
    (= (tree-size '(+ 1 ((2)))) 3)
    (= (tree-size '(+ 1 (((2))))) 3)
    ;; Cases which justify the comment note
    (not (= (tree-size '(+ 1 (((2 3))))) 3))
    (not (= (tree-size '(+ 1 (((2 3)) 4))) 4))
    (not (= (tree-size '(+ 1 (((2 3)) 4 5 6))) 6))
    (not (= (tree-size '(+ 1 (((2 3) 4) 5 6 7))) 6))))

(defmethod test-create-expresions-n ((o test-core-functions))
  "Check expression creation from size 1 to <o> max-size."
  (let* ((algorithm (default-algorithm o))
         (language (language algorithm)))
    (dotimes (j 10)
      (let ((k (1+ j)))
        (format *test-output* "~%~%*** Expressions for size ~A~%" k)
        (setf (max-depth language) k
              (max-size language) k
              (max-size-new-individuals language) k)
        (dotimes (i 100) 
          (let ((expresion (create-expresion language k k t t)))
            (format *test-output* "~T~A~60T~A~%" expresion (tree-size expresion))
            (check (<= (tree-size expresion) k))))))))

(defmethod test-get-internal-subtree ((o test-core-functions))
  "Check #'get-internal-subtree basic behavior with examples."
  (let* ((algorithm (default-algorithm o))
         (language (language algorithm)))
    (check 
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 1 language) 
             '(* (sin (cos x)) (+ 3 4 5 6 7)))
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 2 language) '(sin (cos x)))
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 3 language) '(cos x))
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 4 language) 'x)
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 5 language) '(+ 3 4 5 6 7))
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 6 language) '3)
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 7 language) '4)
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 8 language) '5)
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 9 language) '6)
      (equal (get-internal-subtree '(* (sin (cos x)) (+ 3 4 5 6 7)) 10 language) '7))))

(defmethod test-count-internal-subtrees ((o test-core-functions))
  "Test #'count-internal-subtrees with examples."
  (let* ((algorithm (default-algorithm o))
         (language (language algorithm)))
    (check 
      (= (count-internal-subtrees '() language) 1)
      (= (count-internal-subtrees '(+) language) 1)
      (= (count-internal-subtrees '(+ 1) language) 2)
      (= (count-internal-subtrees '(+ 1 2) language) 3)
      (= (count-internal-subtrees '(+ (* x y) 3) language) 5)
      (= (count-internal-subtrees '(+ (* x y) (* z w)) language) 7)
      (= (count-internal-subtrees '(sin (cos (sin (+ 1 2)))) language)))))

(defmethod test-replace-subtree ((o test-core-functions))
  "Test #'replace-subtree with examples."
  (check
    (replace-subtree '(1 x 3 (+ (* x y z) 2) 4) '(+ 5 55 x) 2)               ;(en terminal una lista)
    (replace-subtree '(1 x 3 (+ (* x y z) 2) 4) 222 2)                       ;(en terminal un terminal)
    (replace-subtree '(1 x 3 (+ (* x y z) 2) 4) 222 4)                       ;(en lista una lista)
    (replace-subtree '(1 x 3 (+ (* x y z) 2) 4) 44 5)                        ;(en lista un terminal)
    (replace-subtree '(1 x 3 (+ (* x y z) 2) 4) 222 122)))

(defmethod test-replace-subtree-execution ((o test-core-functions))
  "Test #'replace-subtree execution with an example."
  (dotimes (i 20) 
    (check (not (null (replace-subtree '(+ x 3 (+ (* x z) 2) 4) '(+ 1 2) i))))))

(defmethod test-random-real ((o test-core-functions))
  "Test #'random-real generation range."
  (loop for x from -10 to 10 by 0.5 do
        (loop for y from -10 to 10 by 0.5 do
              (if (<= x y)
                  (dotimes (i 5)
                    (let ((number (random-real x y)))
                      (check 
                        (>= number x)
                        (<= number y))))))))

(defmethod test-random-integer ((o test-core-functions))
  "Test #'random-integer generation range."
  (loop for x from -10 to 10 by 1 do
        (loop for y from -10 to 10 by 1 do
              (if (<= x y)
                  (dotimes (i 5)
                    (let ((number (random-integer x y)))
                      (check 
                        (>= number x)
                        (<= number y)
                        (integerp number))))))))

(defmethod test-create-constant-fixed ((o test-core-functions))
  "Test #'create-constant with an example."
  (let* ((algorithm (default-algorithm o))
         (language (language algorithm))
         (strategy (constants-strategy language)))
    (setf (constants-set strategy) '(1 2 3 e pi))
    (dotimes (i 100)
      (check (find (create-constant strategy) 
                   (constants-set strategy))))))

(defmethod test-random-element-priority-index ((o test-core-functions))
  (dotimes (i 100)
    (check
      (= 3 
         (random-element-priority-weigth-function
          '(1 2 3)
          (lambda (list position value)
            (declare (ignore list position))
            (if (equal value 3) 1 0)))))))

#|
;; #TODO
(defmethod test-equals ((o test-core-functions))
  nil)

(defmethod test-get-function-with-max-arguments ((o test-core-functions))
  nil)
|#