
(defun test-opencl ()
  (ocl::initialize-opencl-fli)
  (ocl::|clFlush| nil))

