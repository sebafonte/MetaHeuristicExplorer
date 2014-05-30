
(defclass operator-performance-test (performance-test-case)
  ())


(defmethod set-up ((o operator-performance-test))
  (reset-random-state o))

(defmethod reset-random-state ((o operator-performance-test))
  (setf *seed* 1))

(defmethod default-algorithm ((o operator-performance-test) &optional &key (class 'generational-algorithm))
  "Answer default algorithm for <o>."
  (let* ((algorithm (make-instance class))
         (task (make-instance 'search-task :algorithm algorithm)))
    (setf (language task) (system-get 'lisp-math-function-xy))
    (prepare-size-in-algorithm algorithm 500)
    algorithm))

(defmethod performance-test-crossover-cfg ((o operator-performance-test))
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'crossover-cfg))
         (language (language algorithm)))
    (timed-performance-test 
     (lambda ()
       (dotimes (i 100)
         (operate operator language 
                  (list '(+ (* 1 (* (+ (* 1 (* (+ (* 1 (* y 3)) (* 2 (* x y))) 3)) 
                                       (* (+ (* 1 (* y 3)) (* 2 (* x y))) (* x y))) 3)) 
                            (* 2 (* x (+ (* 1 (* y 3)) (* 2 (* x y))))))
                        '(+ y (* 1 (* (+ (* (+ (* 1 (* y 3)) (* 2 (* x y))) (* y 3)) 
                                         (* (+ (* 1 (* y 3)) (* 2 (* x y))) (* x y))) 
                                      (* 1 (* y 6))))))))))))

(defmethod performance-test-mutate-cfg ((o operator-performance-test))
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'mutate-cfg))
         (language (language algorithm)))
    (timed-performance-test 
     (lambda ()
       (dotimes (i 100)
         (operate operator language (list '(+ (* (+ (* 1 (* y 3)) (* 2 (* x y))) 
                                                  (* (+ (* 1 (* y 3)) (* 2 (* x y))) 5)) (* 2 (* x y)))
                                           '(+ (* 1 (* (+ (* (+ (* 1 (* (+ (* 1 (* y 3)) (* 2 (* x y))) 3))
                                                                (* 2 (* x y))) (* y 3)) (* 2 (* x y))) 5)) 
                                               (* 2 (* x y))))))))))

(defmethod performance-test-branch-delete-cfg ((o operator-performance-test))
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'branch-delete-cfg))
         (language (language algorithm)))
    (timed-performance-test 
     (lambda ()
       (dotimes (i 100)
         (operate operator language (list '(+ (* 1 (* (+ (* (+ (* 1 (* y (+ (* 1 (* y 3)) (* 2 (* x y)))))
                                                                (* 2 (* x y))) (* y 3)) 
                                                          (* 2 (* (+ (* 1 (* y 3)) (* 2 (* x y))) y))) 3)) 
                                               (* 2 (* x y))))))))))

(defmethod performance-test-random-create-cfg ((o operator-performance-test))
  (let* ((algorithm (default-algorithm o))
         (operator (system-get 'random-create-cfg))
         (language (language algorithm)))  
    (timed-performance-test 
     (lambda ()
       (dotimes (i 500)
         (operate operator language (list nil)))))))
