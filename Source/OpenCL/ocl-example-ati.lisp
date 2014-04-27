;; Constants
(setf *float-size* 4)
(setf *result-buffer-size* 1)
(setf *datatype* 'single-float)

(setf *number-of-samples* #|(* 256 256)|# 16)
(setf *buffer-size* *number-of-samples*)

(setf *lisp-result-buffer* (make-array *buffer-size* :element-type *datatype* :allocation :static))






;; TEST
(defun create-buffer-helper (context size &optional (flags 0) (host-ptr nil))
  (fli:with-dynamic-foreign-objects ((error INT))
    (check2 (|clCreateBuffer| context flags size host-ptr error)
            (fli:dereference error))))

;; Context
(setf *platforms* (ocl:platforms))
(setf *first-platform* (first (ocl:platforms)))
(setf *devices* (ocl:devices *first-platform*))
(setf *context* (ocl:create-context *devices*))


(setf *result-buffer* (create-write-buffer *context* (* *float-size* *result-buffer-size*) *lisp-result-buffer*))




(fli:with-dynamic-foreign-objects
    ((error ocl:buffer))
  (setf (fli:dereference error) *result-buffer*)
  (set-kernel-argument *kernel* 2 4 error))