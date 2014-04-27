
(defun initialize-default-optimization-targets ()
  (system-add 
   (make-instance 'best-of-population-optimization-target
                  :name 'optimization-target-best-1
                  :size 1)
   (make-instance 'random-population-optimization-target
                  :name 'optimization-target-random-1
                  :size 1)
   (make-instance 'population-optimization-target
                  :name 'optimization-target-all-population)))


(defun optimization-targets ()
  (list (system-get 'optimization-target-all-population)
        (system-get 'optimization-target-best-1)
        (system-get 'optimization-target-random-1)))

