
(defclass entity-function-x-cl-evaluator (opencl-evaluator entity-function-x-evaluator)
  ((x-buffer :initarg :x-buffer :accessor x-buffer)
   (y-buffer :initarg :y-buffer :accessor y-buffer)
   (result-buffer :initarg :result-buffer :accessor result-buffer)
   (lisp-x-buffer :initarg :lisp-x-buffer :accessor lisp-x-buffer)
   (lisp-y-buffer :initarg :lisp-y-buffer :accessor lisp-y-buffer)
   (lisp-result-buffer :initarg :result-buffer :accessor lisp-result-buffer)))


(defmethod initialize-properties :after ((o entity-function-x-cl-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :editor 'text-editor :default-value "Function X CL evaluator")
   (:name 'description :label "Description" :accessor-type 'accessor-accessor-type 
    :data-type 'string :editor 'text-editor :default-value "Function X CL evaluator")
   (:name 'target-program :label "Target program" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'lisp-editor
    :default-value '(cos (/- (+ (* x x) (* 2 y y)) 4)) 
    :possible-values '(cos (/- (+ (* x x) (* 2 y y)) 4)))))

(defmethod initialize-buffers ((o entity-function-x-cl-evaluator))
  "Initialize <o> buffers."
  (setf (x-buffer o) (ocl::create-read-buffer (context o) (samples o))
        (y-buffer o) (ocl::create-read-buffer (context o) (samples o))
        (result-buffer o) (ocl::create-write-buffer (context o) (* *float-size* (global-work-size o)))
        (lisp-x-buffer o) (make-array (samples o) :element-type 'single-float :allocation :static)
        (lisp-y-buffer o) (make-array (samples o) :element-type 'single-float :allocation :static)
        (lisp-result-buffer o) (make-array (global-work-size o) :initial-element 0.0 :element-type 'single-float :allocation :static))
  (dotimes (i (samples o))
    (let ((value (aref (fitness-vector o) i)))
      (setf (aref (lisp-x-buffer o) i) (coerce (first value) 'float) 
            (aref (lisp-y-buffer o) i) (coerce (second value) 'float)))))

(defmethod initialize-source-templates ((o entity-function-x-cl-evaluator))
  "Initialize <o> OpenCL code templates for evaluating individuals and populations."
  (setf (program-source-core o) (string-from-file (merge-pathnames "OpenCL\\fitness-base-core.cl" *base-pathname*))
        (program-source-part o) (string-from-file (merge-pathnames "OpenCL\\fitness-base-part-x.cl" *base-pathname*))
        (program-source-core-population o) (string-from-file (merge-pathnames "OpenCL\\fitness-base-core-population.cl" *base-pathname*))
        (program-source-part-population o) (string-from-file (merge-pathnames "OpenCL\\fitness-base-part-population-x.cl" *base-pathname*))))
          
(defmethod evaluate-distance ((e entity-function-x-cl-evaluator) (o entity-function-x))
  "OpenCL individual evaluation."
  (let* ((samples (samples e))
         (source (cl-program-source e o))
         (buffer-size-bytes (* *float-size* (samples e)))
         (program (ocl::build-program-from-source (context e) source "-cl-opt-disable"))
         (kernel (ocl::create-kernel program "Fitness"))
         (result-buffer-size *float-size*)
         (queue (queue e)))
    ;; Create buffer arguments
    (ocl::setf-buffer-argument (context e) (x-buffer e) kernel 0 buffer-size-bytes 34 (lisp-x-buffer e))
    (ocl::setf-buffer-argument (context e) (y-buffer e) kernel 1 buffer-size-bytes 34 (lisp-y-buffer e))
    (ocl::setf-buffer-argument (context e) (result-buffer e) kernel 2 buffer-size-bytes 1 (lisp-result-buffer e))
    ;; Write array with x and y values
    (ocl::check-cl (ocl::|clEnqueueWriteBuffer| queue (x-buffer e) 1 0 buffer-size-bytes (lisp-x-buffer e) 0 nil nil))
    (ocl::check-cl (ocl::|clEnqueueWriteBuffer| queue (y-buffer e) 1 0 buffer-size-bytes (lisp-y-buffer e) 0 nil nil))
    ;; Kernel
    (ocl::enqueue-kernel kernel queue #(1) #(1))
    ;; Read result
    (ocl::check-cl (ocl::|clEnqueueReadBuffer| queue (result-buffer e) 1 0 result-buffer-size (lisp-result-buffer e) 0 nil nil))
    (ocl::|clReleaseKernel| kernel)
    (ocl::|clReleaseProgram| program)
    (setf (fitness o) (nan-replace (/ 10 (1+ (aref (lisp-result-buffer e) 0))) 0))))

(defmethod evaluate-distance ((e entity-function-x-cl-evaluator) (p population))
  "OpenCL population evaluation."
  (let* ((queue (queue e))
         (buffer-size-bytes (* *float-size* (samples e)))
         (groups-size (min (size p) (global-work-size e))))
    ;; Evaluate objects by groups
    (dolist (group (population-groups p groups-size))
      (let* ((result-buffer-size (* *float-size* (length group)))
             (source (cl-program-source e group))
             (program (ocl::build-program-from-source (context e) source "-cl-opt-disable"))
             (kernel (ocl::create-kernel program "Fitness")))
        ;; Create buffer arguments
        (ocl::setf-buffer-argument (context e) (x-buffer e) kernel 0 buffer-size-bytes 34 (lisp-x-buffer e))
        (ocl::setf-buffer-argument (context e) (y-buffer e) kernel 1 buffer-size-bytes 34 (lisp-y-buffer e))
        (ocl::setf-buffer-argument (context e) (result-buffer e) kernel 2 result-buffer-size 1 (lisp-result-buffer e))
        ;; Write array with x and y values
        (ocl::check-cl (ocl::|clEnqueueWriteBuffer| queue (x-buffer e) 1 0 buffer-size-bytes (lisp-x-buffer e) 0 nil nil))
        (ocl::check-cl (ocl::|clEnqueueWriteBuffer| queue (y-buffer e) 1 0 buffer-size-bytes (lisp-y-buffer e) 0 nil nil))
        ;; Kernel
        (ocl::enqueue-kernel kernel queue (make-array '(1) :initial-element (global-work-size e)) (make-array '(1) :initial-element (local-work-size e)))
        ;; Read result
        (ocl::check-cl (ocl::|clEnqueueReadBuffer| queue (result-buffer e) 1 0 result-buffer-size (lisp-result-buffer e) 0 nil nil))
        (ocl::|clReleaseKernel| kernel)
        (ocl::|clReleaseProgram| program)
        (let ((index 0))
          (dolist (i group)
            (setf (fitness i) (nan-replace (/ 10 (1+ (aref (lisp-result-buffer e) index))) 0))
            (incf index)))))))

(defmethod samples-xmin ((e entity-function-x-cl-evaluator))
  (apply 'min (to-list (lisp-x-buffer e))))

(defmethod samples-xmax ((e entity-function-x-cl-evaluator))
  (apply 'max (to-list (lisp-x-buffer e))))

(defmethod samples-ymin ((e entity-function-x-cl-evaluator))
  (apply 'min (to-list (lisp-y-buffer e))))

(defmethod samples-ymax ((e entity-function-x-cl-evaluator))
  (apply 'max (to-list (lisp-y-buffer e))))

(defmethod samples-list ((e entity-function-x-cl-evaluator))
  '((1 1) (2 2)))

(defmethod reset-temporary-data :after ((e entity-function-x-cl-evaluator))
  "Clear temporary data to be used for evaluation (arrays, flags, etc)."
  (ocl::|clReleaseMemObject| (x-buffer e))
  (ocl::|clReleaseMemObject| (y-buffer e))
  (ocl::|clReleaseMemObject| (result-buffer e))
  (setf (x-buffer e) nil
        (y-buffer e) nil
        (result-buffer e) nil))