
;;; Strategies for LISP SUB EXPs
;;; --------------------------------
;;;  - Steepest descent for constants  	X
;;;  - Round constants 		           	X
;;;  - Delete term    
;;;  - Reduce known expressions
;;;
;;; Strategies for POLYNOMIALS
;;; ---------------------------
;;;  - Merge terms
;;;  - Steepest descent for constants   X
;;;  - Round constants            	X
;;;  - Delete term		               	X
;;;
;;; Strategies for VRP
;;; --------------------
;;;  - 2-opt                           	X
;;;  - 3-opt                           	X
;;;
;;; Strategies for LINEAR ORDERING
;;; --------------------------------
;;;  - Compact interchanges list		X
;;;

(defun initialize-default-optimization-methods ()
  (system-add
   ;; Steepest descent constant optimization
   (make-instance 'steepest-descent-optimization
                  :name 'optimization-method-steepest-descent
                  :value-function #'optimize-constants-steepest-descent-1
                  :delta-gradient 0.01
                  :precision 0.00001
                  :max-iterations 200)
   ;; Round constants (greedy)
   (make-instance 'optimization-method
                  :name 'optimization-method-round-constants
                  :value-function #'optimize-round-constants)
   ;; Steepest descent -> Round constants (greedy)
   (make-instance 'composite-optimization-method
                  :name 'optimization-method-steepest-descent-round
                  :method-list (list (make-instance 'steepest-descent-optimization 
                                                    :name 'optimization-method-steepest-descent
                                                    :value-function #'optimize-constants-steepest-descent-1
                                                    :delta-gradient 0.01
                                                    :precision 0.00001
                                                    :max-iterations 200)
                                     (make-instance 'optimization-method 
                                                    :name 'optimization-method-round-constants
                                                    :value-function #'optimize-round-constants)))
   ;; Steepest descent -> Steepest descent-1 -> Round constants (greedy)
   (make-instance 'composite-optimization-method
                  :name 'optimization-method-steepest-descent-steepest-descent-1-round
                  :method-list (list (make-instance 'steepest-descent-optimization 
                                                    :name 'optimization-method-steepest-descent
                                                    :value-function #'optimize-constants-steepest-descent-1
                                                    :delta-gradient 0.01
                                                    :precision 0.00001
                                                    :max-iterations 200)
                                     (make-instance 'steepest-descent-optimization 
                                                    :name 'optimization-method-steepest-descent
                                                    :value-function #'optimize-constants-steepest-descent-2
                                                    :delta-gradient 0.01
                                                    :precision 0.00001
                                                    :max-iterations 200)
                                     (make-instance 'optimization-method 
                                                    :name 'optimization-method-round-constants
                                                    :value-function #'optimize-round-constants)))
   ;;; Tree optimizations
   (make-instance 'optimization-method
                  :name 'optimize-lisp-math-simplification
                  :value-function #'optimize-lisp-math-simplification)
   ;; Polynomial optimizations
   (make-instance 'optimization-method
                  :name 'optimize-polynomial-delete-term
                  :value-function #'optimize-polynomial-delete-term)
   ;; Pattern simplification
   (make-instance 'pattern-simplification
                  :name 'optimize-lisp-math-patterns
                  :value-function #'optimize-lisp-math-patterns)
   (make-instance 'pattern-simplification
                  :name 'optimize-polynomial-patterns
                  :value-function #'optimize-polynomial-patterns)
   ;; VRP optimizations
   (make-instance 'optimization-method
                  :name 'optimize-vrp-2-opt
                  :value-function #'optimize-vrp-2-opt)
   (make-instance 'optimization-method
                  :name 'optimize-vrp-3-opt
                  :value-function #'optimize-vrp-3-opt)
   ;; Linear ordering optimizations
   (make-instance 'optimization-method
                  :name 'optimize-lop-list-compact
                  :value-function #'optimize-lop-list-compact)))


(defun optimization-methods ()
  (list 
   ;; Constant optimization methods
   (system-get 'optimization-method-steepest-descent)
   (system-get 'optimization-method-round-constants) 
   (system-get 'optimization-method-steepest-descent-round)
   (system-get 'optimization-method-steepest-descent-steepest-descent-1-round)
   ;; Expression optimization methods
   (system-get 'optimize-lisp-math-simplification)
   (system-get 'optimize-polynomial-delete-term)
   (system-get 'optimize-lisp-math-patterns)
   ;; Special optimization methods
   (system-get 'optimize-polynomial-patterns)
   (system-get 'optimize-vrp-2-opt)
   (system-get 'optimize-vrp-3-opt)
   (system-get 'optimize-lop-list-compact)))
