(defparameter *float-size* 4)


(defclass opencl-evaluator (object-with-properties)
  ((global-work-size :initarg :global-work-size :initform (default-global-work-size) :accessor global-work-size)
   (local-work-size :initarg :local-work-size :initform (default-local-work-size) :accessor local-work-size)
   (language :initarg :language :accessor language)
   (program-source-core :initarg :program-source-core :accessor program-source-core)
   (program-source-part :initarg :program-source-part :accessor program-source-part)
   (program-source-core-population :initarg :program-source-core-population :accessor program-source-core-population)
   (program-source-part-population :initarg :program-source-part-population :accessor program-source-part-population)))


(defmethod platform ((o opencl-evaluator))
  *default-cl-platform*)

(defmethod device ((o opencl-evaluator))
  *default-cl-device*)

(defmethod queue ((o opencl-evaluator))
  *default-cl-queue*)

(defmethod context ((o opencl-evaluator))
  *default-cl-context*)


;; #TODO: Get maximum values from ocl-utility
(defmethod default-global-work-size ()
  256)

(defmethod default-local-work-size ()
  4)

(defmethod initialize-properties :after ((o opencl-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'language :label "Language" :accessor-type 'accessor-accessor-type :data-type 'object 
    :possible-values (list (system-get 'lisp-cl-translation-language)) :default-value (system-get 'lisp-cl-translation-language) 
    :editor 'list-editor)))

#|
(defmethod initialize-instance :after ((o opencl-evaluator) &rest args)
  (initialize-context o)
  o)

(defmethod initialize-context ((o opencl-evaluator))
  (setf (context o) *default-cl-context*)
        (device o) *default-cl-device*
        (queue o) *default-cl-queue*)

(defmethod reset-temporary-data :after ((o opencl-evaluator))
  "Clear temporary data to be used for evaluation, suck as array, flags, etc."
  (free-context o))

(defmethod free-context ((o opencl-evaluator))
  (when (context o)
    (ocl::|clReleaseContext| (context o))
    ;(ocl::|clReleaseQueue| (queue o))
    (setf (context o) nil
          (queue o) nil)))
|#

(defmethod initialize-buffers ((o opencl-evaluator))
  "Initialize <o> buffers."
  (error "Subclass responsibility"))

(defmethod parse-to-c-function ((o opencl-evaluator) program)
  (get-cl-expression (parse (grammar (language o)) program)))

(defmethod cl-program-parts ((e opencl-evaluator) (o list))
  (let ((result))
    (dolist (i o)
      (appendf result (list (cl-program-part e i))))
    result))

(defmethod cl-program-parts-population ((e opencl-evaluator) (o list))
  (let ((result '("if (false) {} "))
        (id 0))
    (dolist (i o)
      (appendf result (list (cl-program-part-population e i id)))
      (incf id))
    result))

(defmethod cl-program-part ((e opencl-evaluator) (o t))
  (format nil (program-source-part e) (samples e) (fitness-function-operator e) (parse-to-c-function e (program o))))

(defmethod cl-program-part-population ((e opencl-evaluator) (o t) id)
  (format nil (program-source-part-population e) id (samples e) (parse-to-c-function e (program o))))

(defmethod cl-program-source ((e opencl-evaluator) (o population))
  (cl-program-source e (individuals o)))

(defmethod cl-program-source ((e opencl-evaluator) (o list))
  (format nil (program-source-core-population e) "Fitness" (cl-program-parts-population e o)))

(defmethod cl-program-source ((e opencl-evaluator) (o t))
  (format nil (program-source-core e) "Fitness" (cl-program-parts e (list o))))

(defmethod initialize-fitness-data :after ((o opencl-evaluator))
  (initialize-source-templates o)
  (initialize-buffers o))

(defmethod initialize-source-templates ((o opencl-evaluator))
  (error "Subclass responsibility"))

(defmethod population-groups ((p population) size)
  (let ((list (individuals p))
        (result)
        (group))
    (dolist (i list)
      (appendf group (list i))
      (when (= size (length group))
        (appendf result (list group))
        (setf group nil)))
    (if (> (length group) 0)
      (appendf result (list group)))
    result))

(defmethod evaluate ((e opencl-evaluator) (o population))
  "Use <evaluator> to calculate and answer <o> fitness."
  (funcall (fitness-function e) e o))

(defmethod fitness-function-operator ((e opencl-evaluator))
  "Answer the OpenCL function name for <e> fitness function.
   #NOTE: Intended for point's acumulation with a function over each sum."
  (case (fitness-function e)
    ('evaluate-distance "fabs")
    ('evaluate-distance-squared "sqr")
    (:otherwise (fitness-function e))))

(defmethod excluded-slot-name ((o opencl-evaluator) slot-name)
  (or (equal slot-name 'context)
      (equal slot-name 'platform)
      (equal slot-name 'device)
      (equal slot-name 'queue)))

(defmethod source-description ((o ocl:platform))
  nil)

(defmethod source-description ((o fli::pointer))
  nil)
