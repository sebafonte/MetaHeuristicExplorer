
(defun convert-strings-to-foreign-array (strings &key (allocation :static))
  (let* ((count (length strings))
         (array (fli:allocate-foreign-object 
                 :type '(:pointer (:unsigned :char))
                 :nelems (1+ count)
                 :initial-element nil
                 ;:allocation allocation
                 )))
    (loop for index from 0
          for string in strings
          do (setf (fli:dereference array :index index)
                   (fli:convert-to-foreign-string
                    string
                    :external-format :utf-8
                    ;:allocation allocation
                    )))
    array))

(setf *program-source*
      "__kernel void VectorAdd(__global const float* a, __global const float* b, __global float* c)
{
    // Get index into global data array
    int iGID = get_global_id(0);
    // Compute result value
    float x = a[iGID];
	float y = sin(x);
	c[0] = abs(b[iGID] - y);
}")

(fli:with-dynamic-foreign-objects 
    ((size :unsigned (length *program-source*))
     (error ocl::int))
  (let ((array (convert-strings-to-foreign-array (list *program-source*) :allocation :dynamic)))
    (ocl::|clCreateProgramWithSource| *context* 1 array size error)
    error))

