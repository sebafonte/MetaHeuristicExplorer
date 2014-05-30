
;; Testeo de dependientes con task
(setf task (make-instance 'search-task))
(set-value-for-property-named task 'objetive-class 'entity-sample-vrp)
(list (fitness-evaluator task) (language task) (initialization-method (algorithm task)))
(set-value-for-property-named task 'objetive-class 'entity-function-maximization)
(list (fitness-evaluator task) (language task) (initialization-method (algorithm task)))
(set-value-for-property-named task 'objetive-class 'entity-sample-vrp)
(list (fitness-evaluator task) (language task) (initialization-method (algorithm task)))
(set-value-for-property-named task 'objetive-class 'entity-function-x)
(list (fitness-evaluator task) (language task) (initialization-method (algorithm task)))


(time (dotimes (i 10000) (make-instance 'search-task)))
;; Antes                        40
;; Ahora                        56
;; Ahora con optimización       
