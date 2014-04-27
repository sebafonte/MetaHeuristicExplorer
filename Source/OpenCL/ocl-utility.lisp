
(defvar *default-cl-platforms* nil)
(defvar *default-cl-devices* nil)

(defvar *default-cl-platform* nil)
(defvar *default-cl-device* nil)
(defvar *default-cl-context* nil)
(defvar *default-cl-queue* nil)


#|
;;; #TODO: Move to new class opencl-profile which can be assigned to each fitness evaluator

   (context :initform nil :initarg :context :accessor context)
   (device :initarg :device :accessor device)
   (queue :initarg :queue :accessor queue)

   (:name 'platform :label "Platform" :accessor-type 'accessor-accessor-type :data-type 'object 
    :possible-values platforms :default-value first-platform :editor 'list-editor)
   (:name 'device :label "Device" :accessor-type 'accessor-accessor-type :data-type 'object 
    :possible-values devices :default-value first-device :editor 'list-editor)
|#


(defclass opencl-utility ()
  ())


(defun initialize-opencl ()
  (ocl:initialize-opencl-fli)
  (when (ocl:opencl-available)
    (initialize-opecl-environment)
    (initialize-default-cl-platform)
    (initialize-default-cl-context)
    (initialize-default-cl-device)
    (initialize-default-cl-queue)))

(defun initialize-opecl-environment ()
  (setf *default-cl-platforms* (ocl:platforms)
        *default-cl-devices* (make-hash-table))
  (dolist (platform *default-cl-platforms*)
    (dolist (device (ocl:devices platform))
      (appendf (gethash platform *default-cl-devices*) (list device)))))

(defun initialize-default-cl-platform ()
  (setf *default-cl-platform* (first *default-cl-platforms*)))

(defun initialize-default-cl-device ()
  (setf *default-cl-device* (first (gethash *default-cl-platform* *default-cl-devices*))))

(defun initialize-default-cl-context ()
  (setf *default-cl-context* (ocl:create-context (ocl:devices *default-cl-platform*))))

(defun initialize-default-cl-queue ()
  (setf *default-cl-queue* (ocl:create-queue *default-cl-context* *default-cl-device*)))

(defmethod opencl-available-p ()
  t)

(defmethod device-info-string (device)
  (format nil "" nil))

;; clGetDeviceInfo
(defmethod device-global-memory (device)
  "CL_DEVICE_GLOBAL_MEM_SIZE"
  (ocl:get-param-info 'ocl::DEVICE-GLOBAL-MEM-SIZE device))

(defmethod device-local-memory (device)
  "CL_DEVICE_LOCAL_MEM_SIZE"
  (ocl:get-param-info 'ocl::DEVICE-LOCAL-MEM-SIZE device))

(defmethod device-local-memory-type (device)
  "CL_DEVICE_LOCAL_MEM_TYPE"
  (ocl:get-param-info 'ocl::DEVICE-LOCAL-MEM-TYPE device))

(defmethod device-compiler-available (device)
  "CL_DEVICE_COMPILER_AVAILABLE"
  (ocl:get-param-info 'ocl::DEVICE-COMPILER-AVAILABLE device))

(defmethod device-extensions (device)
  "CL_DEVICE_EXTENSIONS"
  (ocl:get-param-info 'ocl::DEVICE-EXTENSIONS device))

(defmethod device-max-work-items-dimensions (device)
  "CL_MAX_WORK_ITEM_DIMENSIONS"
  (ocl:get-param-info 'ocl::MAX-WORK-ITEM-DIMENSIONS device))

(defmethod device-image-support-p (device)
  "CL_DEVICE_IMAGE_SUPPORT"
  (ocl:get-param-info 'ocl::DEVICE-IMAGE-SUPPORT device))

(defmethod device-image-max-height (device)
  "CL_DEVICE_IMAGE2D_MAX_HEIGHT"
  (ocl:get-param-info 'ocl::DEVICE-IMAGE2D-MAX-HEIGHT device))

(defmethod device-image-max-width (device)
  "CL_DEVICE_IMAGE2D_MAX_WIDTH"
  (ocl:get-param-info 'ocl::DEVICE-IMAGE2D-MAX-WIDTH device))

(defmethod device-max-clock (device)
  "CL_DEVICE_MAX_CLOCK_FREQUENCY"
  (ocl:get-param-info 'ocl::DEVICE-MAX-CLOCK-FREQUENCY device))

(defmethod device-max-compute-units (device)
  "CL_DEVICE_MAX_COMPUTE_UNITS"
  (ocl:get-param-info 'ocl::MAX-COMPUTE-UNITS device))

(defmethod device-profile (device)
  "CL_DEVICE_PROFILE"
  (ocl:get-param-info 'ocl::DEVICE-PROFILE device))

(defmethod device-profiling-timer-resolution (device)
  "CL_DEVICE_PROFILING_TIMER_RESOLUTION"
  (ocl:get-param-info 'ocl::DEVICE-PROFILING-TIMER-RESOLUTION device))

(defmethod device-opencl-version (device)
  "CL_DEVICE_OPENCL_C_VERSION"
  (ocl:get-param-info 'ocl::DEVICE-OPENCL-C-VERSION device))

(defmethod device-type (device)
  "CL_DEVICE_TYPE"
  (ocl:get-param-info 'ocl::DEVICE-TYPE device))

(defmethod device-name (device)
  "CL_DEVICE_NAME"
  (ocl:get-param-info 'ocl::DEVICE-NAME device))

(defmethod device-vendor (device)
  "CL_DEVICE_VENDOR"
  (ocl:get-param-info 'ocl::DEVICE-VENDOR device))

(defmethod device-vendor-id (device)
  "CL_DEVICE_VENDOR_ID"
  (ocl:get-param-info 'ocl::DEVICE-VENDOR-ID device))

(defmethod device-address-bits (device)
  "CL_DEVICE_VENDOR_ID"
  (ocl:get-param-info 'ocl::address-bits device))


;; clGetKernelWorkGroupInfo
(defmethod kernel-max-work-items (device kernel)
  (list 0 0 0))

(defmethod kernel-work-group-size (device kernel)
  "CL_KERNEL_WORK_GROUP_SIZE"
  nil)

(defmethod kernel-local-mem-size (device kernel)
  "CL_KERNEL_LOCAL_MEM_SIZE"
  nil)

;; print-object's
(defmethod print-object ((o ocl:platform) seq)
  (format seq "~A" (ocl::name o)))

(defmethod print-object ((o ocl:device) seq)
  (format seq "~A: ~S" #| (device-type o) |# "TYPE" (ocl::name o)))

(defmethod initialize-instance :after ((o ocl:platform) &rest args)
  (setf (ocl::name o)  "platform"))

(defmethod initialize-instance :after ((o ocl:device) &rest args)
  (setf (ocl::name o) (device-name o)))


#|
;; #TODO
(defmethod device-available-memory (device)
  0)
|#
