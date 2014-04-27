
(defun initialize-default-constant-factory ()
  (system-add 
   ;; Real ephemeral constants factory
   (make-instance 'ephemeral-random-constants-factory 
                  :name 'default-ephemeral-0-1d
                  :min-value 0
                  :max-value 1
                  :result-type 'double)
   (make-instance 'ephemeral-random-constants-factory 
                  :name 'default-ephemeral-0-5d
                  :min-value 0
                  :max-value 5
                  :result-type 'double)
   (make-instance 'ephemeral-random-constants-factory 
                  :name 'default-ephemeral-0-10d
                  :min-value 0
                  :max-value 10
                  :result-type 'double)
   ;; Fixed set constants factory
   (make-instance 'fixed-set-constants-factory 
                  :name 'default-fixed-set-numerical-1
                  :constants-set '(0 1 2 3 4 5 6 7 8 9 10))
   (make-instance 'fixed-set-constants-factory 
                  :name 'default-fixed-set-numerical-2
                  :constants-set '(0 1 2 3 4 5 6 7 8 9 10))
   (make-instance 'fixed-set-constants-factory 
                  :name 'default-fixed-set-numerical-3
                  :constants-set '(0 1 2 3 4 5 6 7 8 9))))


(defun default-constant-factory-symbolic-regression ()
  (list (system-get-copy 'default-ephemeral-0-1d)
        (system-get-copy 'default-ephemeral-0-5d)
        (system-get-copy 'default-ephemeral-0-10d)
        (system-get-copy 'default-fixed-set-numerical-1)
        (system-get-copy 'default-fixed-set-numerical-2)
        (system-get-copy 'default-fixed-set-numerical-3)))

