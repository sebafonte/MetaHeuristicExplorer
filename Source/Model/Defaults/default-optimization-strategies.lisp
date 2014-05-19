
(defun initialize-default-optimization-strategies ()
  (system-add
   ;; Basic instances
   (make-instance 'optimization-strategy
                  :name 'default-optimization-strategy
                  :description "Default")
   (make-instance 'conditional-optimization-strategy 
                  :name 'default-conditional-optimization-strategy
                  :description "Default conditional optimization")
   ;; Instances for expression search
   (make-instance 'iterational-stage-optimization-strategy 
                  :name 'default-iterational-optimization-strategy
                  :description "Default iterational optimization"
                  :max-iterations 20
                  :optimization-method (system-get 'optimization-method-steepest-descent))
   (make-instance 'generational-stage-optimization-strategy 
                  :name 'default-generational-optimization-strategy
                  :description "Default generational optimization"
                  :max-generations 200
                  :optimization-method (system-get 'optimization-method-steepest-descent))))

(defun optimization-strategies ()
  (list 
   (system-get 'default-optimization-strategy)
   (system-get 'default-generational-optimization-strategy)
   (system-get 'default-iterational-optimization-strategy)
   (system-get 'default-conditional-optimization-strategy)))