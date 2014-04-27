#|
;;; #TODO: 
;;    - Set in c initial values
;;    - Add property opencl-device to 'system-configuration
;;    - Add fitness-evaluator (1d, 2d, serie)
;;    - Add to compute image rgb object
;;    - Add to compute texture deformation object
;;    - Check alternatives to evaluate entire population

(setf *program-source*
"__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) 
{
    int iGID = get_global_id(0);
    float x = a[iGID];
    float y = sin(x);
    c[0] = c[0] + fabs(b[iGID] - y);
}")
|#

(defvar *platforms* (platforms))
(defvar *platform* (first *platforms*))
(defvar *devices* (devices *platform*))
(defvar *context* (create-context *devices*))
(defvar *queues* (loop for device in *devices* collect (create-queue *context* device)))
(defvar *queue* (first *queues*))

#|
;; Testing programs
(setf *program-source*
      "__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) { int iGID = get_global_id(0); c[iGID] = iGID; }")
(setf *program-source*
      "__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) { c[0] = 2332; }")

;; #MAKE ONE WITH SINCRONIZATION OR WORKGROUPS
(setf *program-source*
      "__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) { int iGID = get_global_id(0); c[0] = c[0] + 1; }")

"__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) { int iGID = get_global_id(0); float x = sin(a[iGID]); c[iGID] = fabs(x - b[iGID]); }"
"__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) { int iGID = get_global_id(0); float x = sin(a[iGID]); c[iGID] = fabs(x - b[iGID]); }"
"__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) {  c[get_global_id(0)] = get_global_id(0) + get_local_id(0) * 0.01; }"
|#
        

(defun string-from-file (path)
  (with-open-file (is path)
    (let ((result ""))
      (do ((c (read-char is) 
              (read-char is nil 'the-end)))
          ((not (characterp c)))
        (setf result (concatenate 'string result (string c))))
      result)))

(progn
  (setf ocl::*program-source*
        (string-from-file #P"d:\\temp\\fitness-base.cl")))

(setf *program-source*
      "__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) { c[0] = 2332; }")

#|
(defun eval-opencl-fitness (evaluator object)
  (let ((result))
    (eval-cl-program
     expression
     :args "__kernel void Fitness(__global const float* a, __global const float* b, __global float* c)"
     :body "{ int iGID = get_global_id(0); float x = sin(a[iGID]); c[iGID] = fabs(x - b[iGID]); }"
     :arg-values ((:float (x-values evaluator))
                  (:float (y-values evaluator))
                  (:float result)))
    result))

(defun eval-opencl-fitness (evaluator object)
  (let ((result))
    (eval-cl-program
     expression
     :args "__kernel void Fitness(__global const float* a, __global const float* b, __global float* c)"
     :body "{ int iGID = get_global_id(0); float x = sin(a[iGID]); c[iGID] = fabs(x - b[iGID]); }"
     :arg-values ((:float (x-values evaluator))
                  (:float (y-values evaluator))
                  (:float result)))
    result))
|#
