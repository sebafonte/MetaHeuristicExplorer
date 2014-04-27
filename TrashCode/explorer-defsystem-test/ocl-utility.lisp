
(defclass opencl-utility ()
  ())


(defmethod opencl-available-p ()
  t)

(defmethod devices ()
  nil)

(defmethod default-device ()
  (first devices))

(defmethod device-available-memory (device)
  ""
  0)

(defmethod device-info-string (device)
  (format nil "" nil))

;; clGetDeviceInfo
(defmethod device-global-memory (device)
  "CL_DEVICE_GLOBAL_MEM_SIZE"
  0)

(defmethod device-local-memory (device)
  "CL_DEVICE_LOCAL_MEM_SIZE"
  0)

(defmethod device-local-memory-type (device)
 "CL_DEVICE_LOCAL_MEM_TYPE"
 nil)

(defmethod device-compiler-available (device)
  "CL_DEVICE_COMPILER_AVAILABLE"
  nil)

(defmethod device-extensions (device)
  "CL_DEVICE_EXTENSIONS"
  nil)

(defmethod device-max-work-items-dimensions (device)
  (get-param-info 'max-work-item-dimensions device))

(defmethod device-image-support-p (device)
  "CL_DEVICE_IMAGE_SUPPORT"
  nil)

(defmethod device-image-max-height (device)
  "CL_DEVICE_IMAGE2D_MAX_HEIGHT"
  nil)

(defmethod device-image-max-width (device)
  "CL_DEVICE_IMAGE2D_MAX_WIDTH"
  nil)

(defmethod device-max-clock (device)
  "CL_DEVICE_MAX_CLOCK_FREQUENCY"
  nil)

(defmethod device-max-compute-units (device)
  "CL_DEVICE_MAX_COMPUTE_UNITS"
  (get-param-info 'max-compute-units device))

(defmethod device-profile (device)
  "CL_DEVICE_PROFILE"
  nil)

(defmethod device-profiling-timer-resolution (device)
  "CL_DEVICE_PROFILING_TIMER_RESOLUTION"
  nil)

(defmethod device-type (device)
  "CL_DEVICE_TYPE"
  nil)

(defmethod device-vendor (device)
  "CL_DEVICE_VENDOR"
  nil)

(defmethod device-vendor-id (device)
  "CL_DEVICE_VENDOR_ID"
  nil)

;; clGetKernelWorkGroupInfo
(defmethod kernel-max-work-items (device kernel)
  (list 0 0 0))

(defmethod kernel-work-group-size (device kernel)
  "CL_KERNEL_WORK_GROUP_SIZE"
  nil)

(defmethod kernel-local-mem-size (device kernel)
  "CL_KERNEL_LOCAL_MEM_SIZE"
  nil)


