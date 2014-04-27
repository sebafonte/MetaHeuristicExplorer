

(defsystem defsystem-test
  (:package "USER"
   :default-pathname (pathname-location (current-pathname))
   :default-type :lisp-file)
  :members ("ocl.lisp"
            "ocl-utility.lisp"
            "sample.lisp")
  :rules 
  ((:in-order-to :compile :all
    (:requires (:load :previous)))))
  
(setf *system* (compile-system 'defsystem-test :force t :load t))

(test-opencl)
