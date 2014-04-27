

(defun platforms ()
  "Returns a list with available OpenCL platforms."
  (let ((count (platform-count)))
    (fli:with-dynamic-foreign-objects ((platforms PLATFORM :nelems count))
      (check (|clGetPlatformIDs| count platforms nil)
        (loop for n :below count collect 
              (make-instance 'ocl::platform :platform (fli:dereference platforms :index n)))))))