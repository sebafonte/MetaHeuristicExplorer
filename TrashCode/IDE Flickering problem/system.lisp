
(defsystem ide-problem-example 
  (:package "USER"
   :default-pathname (pathname-location (current-pathname))
   :default-type :lisp-file)
  :members ("sample.lisp")
  :rules 
  ((:in-order-to :compile :all
    (:requires (:load :previous)))))
  
(setf *system* (compile-system 'ide-problem-example :force t :load t))

(test-opencl)
