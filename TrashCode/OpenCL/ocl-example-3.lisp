
(setf ee1 (make-instance 'entity-function-x-cl-evaluator :samples 32 :fitness-function 'evaluate-distance :target-program '(+ x 1)))
(setf ee2 (make-instance 'entity-function-x-evaluator :samples 32 #| 8388608 |# :fitness-function 'evaluate-distance :target-program '(+ x 1)))

(initialize-fitness-data ee1)
(initialize-fitness-data ee2)

(time (my-round-to-3 (evaluate ee1 (make-instance 'entity-function-x :expresion '(+ 1 x)))))
(time (my-round-to-3 (evaluate ee2 (make-instance 'entity-function-x :expresion '(+ 1 x)))))


#|
;; With 8388608 points, 1 only GPU core:
;;  
;;  - Just take advantage of evaluator object memory leak
;;  - 2.54 times faster
;;  - 16 kb vs. 268 MB consing
;;

CL-USER 23 > (time (my-round-to-3 (evaluate ee1 (make-instance 'entity-function-x :expresion '(+ 1.5 x)))))
Timing the evaluation of (MY-ROUND-TO-3 (EVALUATE EE1 (MAKE-INSTANCE (QUOTE ENTITY-FUNCTION-X) :EXPRESION (QUOTE (+ 1.5 X)))))

User time    =        0.951
System time  =        0.343
Elapsed time =        1.119
Allocation   = 16176 bytes
0 Page faults
0.0

CL-USER 23 > (time (my-round-to-3 (evaluate ee2 (make-instance 'entity-function-x :expresion '(+ 1.5 x)))))
Timing the evaluation of (MY-ROUND-TO-3 (EVALUATE EE2 (MAKE-INSTANCE (QUOTE ENTITY-FUNCTION-X) :EXPRESION (QUOTE (+ 1.5 X)))))

User time    =        2.418
System time  =        0.000
Elapsed time =        2.502
Allocation   = 268476292 bytes
0 Page faults
0.0
|#
