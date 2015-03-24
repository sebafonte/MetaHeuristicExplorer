

;; #TEST: Check a mutation #1
(dotimes (i 100)
  (let* ((operation (system-get 'mutate))
         (algorithm (default-algorithm (make-instance 'test-genetic-operators)))
         (exp (operate operation algorithm (list '(+ 1 2 3 4 10 11 12 13 14 15 16 17 18 19 20 21 22 23)))))
      (format t "~A~%~A~%" exp (tree-size exp))))

;; #TEST: Check a mutation point #1
(dotimes (i 100)
  (let* ((operation (system-get 'mutate-point))
         (algorithm (default-algorithm (make-instance 'test-genetic-operators)))
         (exp (operate operation algorithm (list '(+ 1 2 3 4 10 11 12 13 14 15 16 17 18 19 20 21 22 23)))))
      (format t "~A~%~A~%" exp (tree-size exp))))

;; #TEST: Check a crossover #2
(dotimes (i 100) 
  (let ((operation (system-get 'crossover))
        (algorithm (default-algorithm (make-instance 'test-genetic-operators))))
    (multiple-value-bind (x y) 
        (operate operation algorithm (list '(+ x (* x x) (* x x x) (* x x x x))
                                           '(+ y (* y y) (* y y y) (* y y y y))))
      (print x) (print y) nil)))

;; #TEST: Check a crossover #2
(dotimes (i 100) 
  (let ((operation (system-get 'crossover))
        (algorithm (default-algorithm (make-instance 'test-genetic-operators))))
    (multiple-value-bind (x y) 
        (operate operation algorithm (list '(+ x (* x x) (* x x x) (* x x x x))
                                           '(sqrt (+ (* x x) (* y y) (* z z)))))
      (print x) (print y) nil)))


;; See expressions - lisp-math-function-grammar-x-y
(let* ((operator (system-get 'crossover-cfg))
       (grammar (system-get 'lisp-math-function-grammar-x-y))
       (algorithm (make-instance 'generational-algorithm))
       (table (make-hash-table :test 'equal)))
  (setf (grammar algorithm) grammar
        (max-size algorithm) 25)
  (dotimes (i 100)
    (let ((new-value   
           (directed-crossover-cfg '(+ (* 2 (* x x)) (* 3 (* x x)))
                                   '(- 4 (/ 5 (/ y y)))
                                   algorithm
                                   operator)))
      (setf (gethash new-value table) (size new-value))))
  table)

;; See expressions and frequency
(let* ((operator (system-get 'crossover-cfg))
       (grammar (system-get 'lisp-math-function-grammar-x-y))
       (algorithm (make-instance 'generational-algorithm))
       (table (make-hash-table :test 'equal)))
  (setf (grammar algorithm) grammar
        (max-size algorithm) 25)
  (dotimes (i 1000)
    (let* ((new-value   
            (directed-crossover-cfg '(+ (* 2 (* x x)) (* 3 (* x x)))
                                    '(- 4 (/ 5 (/ y y)))
                                    algorithm
                                    operator))
           (times (gethash new-value table)))
      (setf (gethash new-value table) 
            (if (null times) 1 (1+ times)))))
  table)

;; See expressions size and frequency
(let* ((operator (system-get 'crossover-cfg))
       (grammar (system-get 'lisp-math-function-grammar-x-y))
       (algorithm (make-instance 'generational-algorithm))
       (table (make-hash-table :test 'equal)))
  (setf (grammar algorithm) grammar
        (max-size algorithm) 50)
  (dotimes (i 1000)
    (let* ((new-value   
            (directed-crossover-cfg '(+ (* 2 (* x x)) (* 3 (* x x)))
                                    '(- 4 (/ 5 (/ y y)))
                                    algorithm
                                    operator))
           (times (gethash (size new-value) table)))
      (setf (gethash (size new-value) table) 
            (if (null times) 1 (1+ times)))))
  table)


;; See expressions - polinomyal-grammar-x-y
(let* ((operator (system-get 'crossover-cfg))
       (grammar (system-get 'polinomyal-grammar-x-y))
       (algorithm (make-instance 'generational-algorithm))
       (table (make-hash-table :test 'equal)))
  (setf (grammar algorithm) grammar)
  (setf (max-size algorithm) 25)
  (dotimes (i 100)
    (let ((new-value   
           (directed-crossover-cfg '(+ 1 (* 2 (* x x)) (* 3 (* x x x)))
                                   '(+ 4 (* 5 (* y y)))
                                   algorithm
                                   operator)))
      (setf (gethash new-value table) (size new-value))))
  table)


;;; 
(let* ((operator (system-get 'crossover-cfg))
       (language (system-get 'search-task-default-language))
       (task (make-instance 'search-task))
       (algorithm (make-instance 'generational-algorithm))
       (table (make-hash-table :test 'equal))
       (t1 '(BEST-OF-TASKS
             (MAKE-TASK
              (MAKE-BUILDER-IT 1)
              (MAKE-ALG-GG 100 53 (MAKE-SM-TOURNAMENT 5) (MAKE-EM 5))
              (MAKE-LG 40 0 10)
              (MAKE-GN-RND 1)
              (MAKE-FE))
             (MAKE-TASK
              (MAKE-BUILDER-IT 1)
              (MAKE-ALG-GG 100 53 (MAKE-SM-TOURNAMENT 5) (MAKE-EM 5))
              (MAKE-LG 40 0 10)
              (MAKE-GN-RND 1)
              (MAKE-FE))))
       (t2 '(BEST-OF-TASKS
             (MAKE-TASK
              (MAKE-BUILDER-IT 5)
              (MAKE-ALG-GG 10 54 (MAKE-SM-TOURNAMENT 4) (MAKE-EM 4))
              (MAKE-LG 40 0 10)
              (MAKE-GN-RND 1)
              (MAKE-FE))
             (MAKE-TASK
              (MAKE-BUILDER-IT 10)
              (MAKE-ALG-GG 20 55 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 3))
              (MAKE-LG 40 0 10)
              (MAKE-GN-RND 1)
              (MAKE-FE)))))
  (setf (context algorithm) (process task)
	    (language (process task)) language
        (max-size language) 200)
  (dotimes (i 100)
    (let ((new-value (directed-crossover-cfg t1 t2 algorithm operator)))
      (setf (gethash new-value table) (tree-size new-value))))
  table)
