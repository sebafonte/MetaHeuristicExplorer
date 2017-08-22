
(defpackage "OCL" (:use "CL")
  (:export
   "PLATFORMS" "PLATFORM-COUNT"
   "DEVICES" "DEVICE-COUNT"
   "OBJECT" "PLATFORM" "DEVICE" "CONTEXT" "QUEUE" "BUFFER" "PROGRAM" "KERNEL" "EVENT" "IMAGE-FORMAT" "SAMPLER"
   "INITIALIZE-OPENCL-FLI"
   "RETAIN" "RELEASE"
   "CREATE-CONTEXT"
   "CREATE-QUEUE"
   "ENQUEUE-COPY-BUFFER"
   "RELEASE-QUEUE" "RELEASE-CONTEXT"
   "GET-PARAM-INFO"
   "OPENCL-AVAILABLE")
   (:import-from "CL-USER"))

(in-package "OCL")


(defvar *opencl-available* nil)
(defvar *opencl-macosx-path* "/System/Library/Frameworks/OpenCL.framework/OpenCL")
(defvar *opencl-windows-path* "C:/Windows/System32/OpenCL.dll")
(defvar *opencl-linux-path* "/usr/lib/libOpenCL.so")


(defclass object    ()         ((object :initarg :object :accessor object)))
(defclass platform  (object)   ((object-devices :initarg :object-devices :accessor object-devices) 
                                (name :initarg :name :initform "" :accessor name)))
(defclass device    (object)   ((name :initarg :name :initform "" :accessor name)))
(defclass context   (object)   ((object-devices :initarg :object-devices :accessor object-devices)))
(defclass queue     (object)   ((device  :initarg :device :accessor device)))
(defclass buffer    (object)   ())
(defclass sampler   (object)   ())
(defclass program   (object)   ((source) (devices)))
(defclass kernel    (object)   ((program) (name)))
(defclass event     (object)   ())
(defclass parameter ()         ((name) (id) (type)))

(defmethod release ((context context)) (|clReleaseContext|   (slot-value context 'object)))
(defmethod release ((queue     queue)) (|clReleaseQueue|     (slot-value queue   'object)))
(defmethod release ((buffer   buffer)) (|clReleaseMemObject| (slot-value buffer  'object)))
(defmethod release ((sampler sampler)) (|clReleaseSampler|   (slot-value sampler 'object)))
(defmethod release ((program program)) (|clReleaseProgram|   (slot-value program 'object)))
(defmethod release ((kernel   kernel)) (|clReleaseKernel|    (slot-value kernel  'object)))
(defmethod release ((event     event)) (|clReleaseEvent|     (slot-value event   'object)))
(defmethod retain  ((context context)) (|clRetainContext|    (slot-value context 'object)))
(defmethod retain  ((queue     queue)) (|clRetainQueue|      (slot-value queue   'object)))
(defmethod retain  ((buffer   buffer)) (|clRetainMemObject|  (slot-value buffer  'object)))
(defmethod retain  ((sampler sampler)) (|clRetainSampler|    (slot-value sampler 'object)))
(defmethod retain  ((program program)) (|clRetainProgram|    (slot-value program 'object)))
(defmethod retain  ((kernel   kernel)) (|clRetainKernel|     (slot-value kernel  'object)))
(defmethod retain  ((event     event)) (|clRetainEvent|      (slot-value event   'object)))


(defparameter *c-types*
  '((STR          (:reference-pass :ef-mb-string))
    (INT          (:signed-integer-type 32))
    (UINT         (:unsigned-integer-type 32))
    (MASK         (:unsigned-integer-type 64))
    (BOOL         UINT)
    (PTR          :pointer)
    (LISP-ARRAY   :lisp-array)
    (PARAM        :signed)
    (SIZE-T       :unsigned)
    (CALLBACK     :pointer)
    (PLATFORM     :pointer)
    (DEVICE       :pointer)
    (CONTEXT      :pointer)
    (QUEUE        :pointer)
    (BUFFER       :pointer)
    (PROGRAM      :pointer)
    (KERNEL       :pointer)
    (EVENT        :pointer)
    (IMAGE-FORMAT :pointer)
    (VOID         :pointer)
    (SAMPLER      :pointer)
    ;; Add on
    (PROGRAM-BUILD-INFO :pointer)))

(defparameter +device-types+
  '(CL_DEVICE_TYPE_CPU
    CL_DEVICE_TYPE_GPU
    CL_DEVICE_TYPE_ACCELERATOR
    CL_DEVICE_TYPE_DEFAULT))

(defparameter +parameters+
  '((platform profile                                  #x0900 STRING)
    (platform version                                  #x0901 STRING)
    (platform name                                     #x0902 STRING)
    (platform vendor                                   #x0903 STRING)
    (platform extensions                               #x0904 STRING)
    (device   type                                     #x1000 MASK)
    (device   vendor-id                                #x1001 UINT)
    (device   max-compute-units                        #x1002 UINT)
    (device   max-work-item-dimensions                 #x1003 UINT)
    (device   max-work-item-sizes                      #x1004 VOID)
    (device   max-work-group-size                      #x1005 SIZE-T)
    (device   preferred-vector-width-char              #x1006 UINT)
    (device   preferred-vector-width-short             #x1007 UINT)
    (device   preferred-vector-width-int               #x1008 UINT)
    (device   preferred-vector-width-long              #x1009 UINT)
    (device   preferred-vector-width-float             #x100A UINT)
    (device   preferred-vector-width-double            #x100B UINT)
    (device   max-clock-frequency                      #x100c UINT)
    (device   address-bits                             #x100d UINT)
    (device   max-mem-alloc-size                       #x100e ULONG)
    (device   image-support                            #x100f UINT)
    (device   max-read-image-args                      #x1010 UINT)
    (device   max-write-image-args                     #x1011 UINT)
    (device   image2d-max-width                        #x1012 SIZE-T)
    (device   image2d-max-height                       #x1013 SIZE-T)
    (device   image3d-max-width                        #x1014 SIZE-T)
    (device   image3d-max-height                       #x1015 SIZE-T)
    (device   image3d-max-depth                        #x1016 SIZE-T)
    (device   max-samplers                             #x1017 UINT)
    (device   max-parameter-size                       #x1018 SIZE-T)
    (device   mem-base-addr-align                      #x1019 UINT)
    (device   min-data-type-align-size                 #x101A UINT)
    (device   single-fp-config                         #x101B ULONG)
    (device   global-mem-cache-type                    #x101C UINT)
    (device   global-mem-cacheline-size                #x101D UINT)
    (device   global-mem-cache-size                    #x101E ULONG)
    (device   global-mem-size                          #x101F ULONG)
    (device   device-max-constant-buffer-size          #x1020 ULONG)
    (device   device-max-constant-args                 #x1021 UINT)
    (device   device-local-mem-type                    #x1022 UINT #| cl_device_local_mem_type |#)
    (device   device-local-mem-size                    #x1023 ULONG)
    (device   device-error-correction-support          #x1024 BOOL)
    (device   device-profiling-timer-resolution        #x1025 SIZE-T)
    (device   device-endian-little                     #x1026 BOOL)
    (device   device-available                         #x1027 BOOL)
    (device   device-compiler-available                #x1028 BOOL)
    (device   device-execution-capabilities            #x1029 UINT #| cl_device_exec_capabilities |#)
    (device   device-queue-properties                  #x102a UINT #| cl_command_queue_properties |#)
    (device   device-name                              #x102b STRING)
    (device   device-vendor                            #x102c STRING)
    (device   driver-version                           #x102d STRING)
    (device   device-profile                           #x102e STRING)
    (device   device-version                           #x102f STRING)
    (device   device-extensions                        #x1030 STRING)
    (device   device-platform                          #x1031 STRING)
    (device   device-double-fp-config                  #x1032 UINT #| cl_device_fp_config |#)
#|
    ;; #x1033 RESERVED for CL-DEVICE-HALF-FP-CONFIG	
    (device   device-preferred-vector-width-half       #x1034)
    (device   device-host-unified-memory               #x1035)
    (device   device-native-vector-width-char          #x1036)
    (device   device-native-vector-width-short         #x1037)
    (device   device-native-vector-width-int           #x1038)
    (device   device-native-vector-width-long          #x1039)
    (device   device-native-vector-width-float         #x103a)
    (device   device-native-vector-width-double        #x103b)
    (device   device-native-vector-width-half          #x103c)
    (device   device-opencl-c-version                  #x103d STRING)
    (device   device-linker-available                  #x103e)
    (device   device-built-in-kernels                  #x103f)
    (device   device-image-max-buffer-size             #x1040)
    (device   device-image-max-array-size              #x1041)
    (device   device-parent-device                     #x1042)
    (device   device-partition-max-sub-devices         #x1043)
    (device   device-partition-properties              #x1044)
    (device   device-partition-affinity-domain         #x1045)
    (device   device-partition-type                    #x1046)
    (device   device-reference-count                   #x1047)
    (device   device-preferred-interop-user-sync       #x1048)
    (device   device-printf-buffer-size                #x1049)
    (device   device-image-pitch-alignment             #x104a)
    (device   device-image-base-address-alignment      #x104b)
|#
    ))
  
(defconstant +error-codes+
  '((SUCCESS                         .   0)
    (DEVICE-NOT-FOUND                .  -1)
    (DEVICE-NOT-AVAILABLE            .  -2)
    (COMPILER-NOT-AVAILABLE          .  -3)
    (BUFFER-ALLOCATION-FAILURE       .  -4)
    (OUT-OF-RESOURCES                .  -5)
    (OUT-OF-HOST-MEMORY              .  -6)
    (PROFILING-NOT-AVAILABLE         .  -7)
    (MEMORY-COPY-OVERLAP             .  -8)
    (IMAGE-FORMAT-MISMATCH           .  -9)
    (IMAGE-FORMAT-NOT-SUPPORTED      . -10)
    (BUILD-PROGRAM-FAILURE           . -11)
    (MAP-FAILURE                     . -12)
    (INVALID-VALUE                   . -30)
    (INVALID-DEVICE-TYPE             . -31)
    (INVALID-PLATFORM                . -32)
    (INVALID-DEVICE                  . -33)
    (INVALID-CONTEXT                 . -34)
    (INVALID-QUEUE-PROPERTIES        . -35)  
    (INVALID-COMMAND-QUEUE           . -36)
    (INVALID-HOST-PTR                . -37)
    (INVALID-MEM-OBJECT              . -38)
    (INVALID-IMAGE-FORMAT-DESCRIPTOR . -39)   
    (INVALID-IMAGE-SIZE              . -40)
    (INVALID-SAMPLER                 . -41)
    (INVALID-BINARY                  . -42)
    (INVALID-BUILD-OPTIONS           . -43)
    (INVALID-PROGRAM                 . -44)
    (INVALID-PROGRAM-EXECUTABLE      . -45)
    (INVALID-KERNEL-NAME             . -46)
    (INVALID-KERNEL-DEFINITION       . -47)
    (INVALID-KERNEL                  . -48)
    (INVALID-ARG-INDEX               . -49)       
    (INVALID-ARG-VALUE               . -50)
    (INVALID-ARG-SIZE                . -51)
    (INVALID-KERNEL-ARGS             . -52)
    (INVALID-WORK-DIMENSION          . -53)
    (INVALID-WORK-GROUP-SIZE         . -54)
    (INVALID-WORK-ITEM-SIZE          . -55)
    (INVALID-GLOBAL-OFFSET           . -56)
    (INVALID-EVENT-WAIT-LIST         . -57)
    (INVALID-EVENT                   . -58)
    (INVALID-OPERATION               . -59)
    (INVALID-GL-OBJECT               . -60)
    (INVALID-BUFFER-SIZE             . -61)
    (INVALID-MIP-LEVEL               . -62)
    (INVALID-GLOBAL-WORK-SIZE        . -63)
    (INVALID-PROPERTY                . -64)
    (INVALID-IMAGE-DESCRIPTOR        . -65)
    (INVALID-COMPILER-OPTIONS        . -66)
    (INVALID-LINKER-OPTIONS          . -67)
    (INVALID-DEVICE-PARTITION-COUNT  . -68)))


(defparameter +c-structures+
  '((IMAGE-FORMAT)))

(defparameter +c-functions+
  '((|clGetPlatformIDs|              INT     ((UINT num-entries) ((PLATFORM) platforms) ((UINT) num-platforms)))
    (|clGetPlatformInfo|             INT     ((PLATFORM platform) (PARAM platform-info) (SIZE-T param-value-size) 
                                              (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clGetDeviceIDs|                INT     ((PLATFORM platform) (MASK device-type) (UINT num-entries) ((DEVICE) devices) ((UINT) num-devices)))
    (|clGetDeviceInfo|               INT     ((DEVICE device) (PARAM device-info) (SIZE-T param-value-size) 
                                              (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clCreateContext|               CONTEXT ((PTR context-properties) (UINT num-devices) ((DEVICE) devices) (CALLBACK notify) (PTR user-data) 
                                              ((INT) error-returned)))
    (|clCreateContextFromType|       CONTEXT ((PTR context-properties) (MASK device-type) (CALLBACK notify) (PTR user-data) ((INT) error-returned)))
    (|clRetainContext|               INT     ((CONTEXT context)))
    (|clReleaseContext|              INT     ((CONTEXT context)))
    (|clGetContextInfo|              INT     ((CONTEXT context) (PARAM context-info) (SIZE-T param-value-size) 
                                              (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clCreateCommandQueue|          QUEUE   ((CONTEXT context) (DEVICE device) (MASK command-queue-properties) ((INT) error-returned)))
    (|clRetainQueue|                 INT     ((QUEUE queue)))
    (|clReleaseQueue|                INT     ((QUEUE queue)))
    (|clGetCommandQueueInfo|         INT     ((QUEUE queue) (PARAM command-queue-info) (SIZE-T param-value-size) 
                                              (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clSetCommandQueueProperty|     INT     ((QUEUE queue) (MASK command-queue-properties) (BOOL enabled) ((MASK) command-queue-properties-returned)))
    (|clCreateBuffer|                BUFFER  ((CONTEXT context) (MASK memory-flags) (SIZE-T memory-size) (LISP-ARRAY host-array) ((INT) error-returned)))
    (|clCreateImage2D|               BUFFER  ((CONTEXT context) (MASK memory-flags) ((IMAGE-FORMAT) format) 
                                              (SIZE-T width) (SIZE-T height) (SIZE-T row-pitch) (LISP-ARRAY host-array) ((INT) error-returned)))
    (|clCreateImage3D|               BUFFER  ((CONTEXT context) (MASK memory-flags) ((IMAGE-FORMAT) format) (SIZE-T width) (SIZE-T height) (SIZE-T depth) 
                                              (SIZE-T row-pitch) (SIZE-T slice-pitch) (LISP-ARRAY host-array) ((INT) error-returned)))
    (|clRetainMemObject|             INT     ((BUFFER buffer)))
    (|clReleaseMemObject|            INT     ((BUFFER bufffer)))
    (|clGetSupportedImageFormats|    INT     ((CONTEXT context) (MASK memory-flags) (PARAM image-type) (UINT num-entries) 
                                              ((IMAGE-FORMAT) image-formats) ((UINT) num-image-formats-returned)))
    (|clGetMemObjectInfo|            INT     ((BUFFER buffer) (PARAM memory-info) (SIZE-T param-value-size) 
                                              (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clGetImageInfo|                INT     ((BUFFER buffer) (PARAM image-info) (SIZE-T param-value-size) 
                                              (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clCreateSampler|               SAMPLER ((CONTEXT context) (BOOL normalized-coords) (UINT addressing-mode) (UINT filter-mode) ((INT) error-returned)))
    (|clRetainSampler|               INT     ((SAMPLER sampler)))
    (|clReleaseSampler|              INT     ((SAMPLER sampler)))
    (|clGetSamplerInfo|              INT     ((SAMPLER sampler) (PARAM sampler-info) (SIZE-T param-value-size) 
                                              (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clCreateProgramWithSource|     PROGRAM ((CONTEXT context) (UINT string-count) (PTR strings) ((SIZE-T) string-lengths) ((INT) error-returned)))
    (|clCreateProgramWithBinary|     PROGRAM ((CONTEXT context) (UINT device-count) ((DEVICE) devices) 
                                              ((SIZE-T) binary-lengths) (PTR binaries) ((INT) binary-status) ((INT) error-returned)))
    (|clRetainProgram|               INT     ((PROGRAM program)))
    (|clReleaseProgram|              INT     ((PROGRAM program)))
    (|clBuildProgram|                INT     ((PROGRAM program) (UINT device-count) ((DEVICE) devices) (STR options) (CALLBACK notify) (PTR user-data)))
    (|clUnloadCompiler|              VOID    )
    (|clGetProgramInfo|              INT     ((PROGRAM program) (PARAM program-info) 
                                              (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-returned)))
    (|clCreateKernel|                KERNEL  ((PROGRAM program) (STR kernel-name) ((INT) error-returned)))
    (|clCreateKernelsInProgram|      KERNEL  ((PROGRAM program) (UINT kernel-count) ((KERNEL) kernels) ((UINT) kernel-count-returned)))
    (|clRetainKernel|                INT     ((KERNEL kernel)))
    (|clReleaseKernel|               INT     ((KERNEL kernel)))
    (|clSetKernelArg|                INT     ((KERNEL kernel) (UINT argument-index) (SIZE-T argument-size) (PTR argument-value)))
    (|clGetKernelInfo|               INT     ((KERNEL kernel) (PARAM kernel-info) 
                                              (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clGetKernelWorkGroupInfo|      INT     ((KERNEL kernel) (DEVICE devicee) (PARAM kernel-workgroup-info) 
                                              (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clWaitForEvents|               INT     ((UINT event-count) ((EVENT) events)))
    (|clGetEventInfo|                INT     ((EVENT event) 
                                              (PARAM event-info (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned))))
    (|clRetainEvent|                 INT     ((EVENT event)))
    (|clReleaseEvent|                INT     ((EVENT event)))
    (|clGetEventProfilingInfo|       INT     ((EVENT event) (PARAM profiling-info) 
                                              (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clFlush|                       INT     ((QUEUE queue)))
    (|clFinish|                      INT     ((QUEUE queue)))
    (|clEnqueueReadBuffer|           INT     ((QUEUE queue) (BUFFER buffer) (BOOL blocking-read) (SIZE-T offset) (SIZE-T cb) (LISP-ARRAY array) 
                                              (UINT waiting-event-count) 
                                              ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueWriteBuffer|          INT     ((QUEUE queue) (BUFFER buffer) (BOOL blocking-write) (SIZE-T offset) (SIZE-T cb) (LISP-ARRAY array) 
                                              (UINT waiting-event-count) 
                                              ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueCopyBuffer|           INT     ((QUEUE queue) (BUFFER src-buffer) (BUFFER dst-buffer) (SIZE-T src-offset) (SIZE-T dst-offset) (SIZE-T size) 
                                              (UINT waiting-event-count) 
                                              ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueReadImage|            INT     ((QUEUE queue) (BUFFER buffer) (BOOL blocking-read)  ((SIZE-T 3) origin) ((SIZE-T 3) region) 
                                              (SIZE-T row-pitch) (SIZE-T slice-pitch) (LISP-ARRAY array) (UINT waiting-event-count) 
                                              ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueWriteImage|           INT     ((QUEUE queue) (BUFFER buffer) (BOOL blocking-write) ((SIZE-T 3) origin) ((SIZE-T 3) region)
                                              (SIZE-T row-pitch) (SIZE-T slice-pitch) (LISP-ARRAY array) (UINT waiting-event-count) 
                                              ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueCopyImage|            INT     ((QUEUE queue) (BUFFER src-buffer) (BUFFER dst-buffer) 
                                              ((SIZE-T 3) src-origin) ((SIZE-T 3) region) (SiZE-T dst-offset)
                                              (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueCopyImageToBuffer|    INT)
    (|clEnqueueCopyBufferToImage|    INT)
    (|clEnqueueMapBuffer|            INT)
    (|clEnqueueMapImage|             INT)
    (|clEnqueueUnmapMemObject|       INT)
    (|clEnqueueTask|                 INT)
    (|clEnqueueNativeKernel|         INT)
    (|clEnqueueMarker|               INT     ((QUEUE queue) ((EVENT) event-returned)))
    (|clEnqueueWaitForEvents|        INT     ((QUEUE queue) (UINT event-count) ((EVENT) events)))
    (|clEnqueueBarrier|              INT     ((QUEUE queue)))
    (|clGetExtensionFunctionAddress| PTR     ((STR function-name)))
    (|clCreateFromGLBuffer|          BUFFER  ((CONTEXT context) (MASK memory-flags) 
                                              (SIZE-T buffer-object) ((INT) error-returned)))
    (|clCreateFromGLTexture2D|       BUFFER  ((CONTEXT context) (MASK memory-flags) 
                                              (SIZE-T texture-target) (SIZE-T mip-level) (SIZE-T texture-object) ((INT) error-returned)))
    (|clCreateFromGLTexture3D|       BUFFER  ((CONTEXT context) (MASK memory-flags) 
                                              (SIZE-T texture-target) (SIZE-T mip-level) (SIZE-T texture-object) ((INT) error-returned)))
    (|clCreateFromGLRenderbuffer|    BUFFER  ((CONTEXT context) (MASK memory-flags) (SIZE-T render-buffer) ((INT) error-returned)))
    (|clGetGLObjectInfo|             INT     ((BUFFER buffer) ((UINT) gl-object-type) ((SIZE-T) gl-object-name)))
    (|clGetGLTextureInfo|            INT     ((BUFFER buffer) (UINT gl-texture-info) 
                                              (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clEnqueueAcquireGLObjects|     INT     ((QUEUE queue) (UINT buffer-count) ((BUFFER) buffers) (UINT waiting-event-count) 
                                              ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clReleaseAcquireGLObjects|     INT     ((QUEUE queue) (UINT buffer-count) ((BUFFER) buffers) (UINT waiting-event-count)
                                              ((EVENT) waiting-events) ((EVENT) event-returned)))
    ;; Create program from source EXT
    (|clCreateProgramWithSource|     PROGRAM ((CONTEXT context) 
                                              (UINT count) 
                                              ((:POINTER (:UNSIGNED :CHAR)) strings) 
                                              ((SIZE-T) program-lenghts) 
                                              ((INT) error-returned)))
    ;; Program build logs
    (|clGetProgramBuildInfo|         INT     ((PROGRAM program)
                                              (DEVICE device)
                                              (PROGRAM-BUILD-INFO build-info)
                                              (SIZE-T param-value-size)
                                              (PTR param-value)
                                              ((SIZE-T) param-value-size-returned)))
    ;; Enqueue kernel
    (|clEnqueueNDRangeKernel|        INT     ((QUEUE queue) (KERNEL kernel) (UINT work-dim)
                                              (SIZE-T global-work-offset) 
                                              ((SIZE-T 3) global-work-size) ((SIZE-T 3) local-work-size)
                                              (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))))

(defvar *opencl-library-path*
  #+win32 *opencl-windows-path*
  #+macosx *opencl-macosx-path*
  #+linux *opencl-linux-path*)

(defun opencl-available ()
  *opencl-available*)

(defun check-opencl-available ()
  (not (null (probe-file *opencl-library-path*))))

(defun initialize-opencl-fli ()
  (setf *opencl-available* (check-opencl-available))
  (when (opencl-available)
    (fli:register-module "OpenCL" :real-name *opencl-library-path* :connection-style :immediate)
    #| (doit) |#))

(defun check (return-code &optional (alternative-return-code nil alternative-return-code-supplied-p))
  "Throws an OpenCL error if the return-code is NEGATIVE, otherwise returns it, or returns an alternative-return-code if supplied.
   Use this with |clXxx| functions return an error."
  (if (< 0 return-code)
      (error "OpenCL error ~A" return-code)
    (if alternative-return-code-supplied-p
        alternative-return-code
      return-code)))

(defun check2 (return-code &optional (error-code nil error-code-supplied-p))
  "Throws an OpenCL error if the return-code is NEGATIVE, otherwise returns it, or returns an alternative-return-code if supplied. 
   Use this with |clXxx| functions returning an object, and returning the error as reference (usually last parameter of the function call)."
  (check (if error-code-supplied-p
             error-code
           return-code) return-code))

(defun check-cl (return-code)
  "Throws an OpenCL error if <return-code> is negative."
  (if (< return-code 0)
      (error "OpenCL error ~A" return-code)))

#|
;; Create bindings for functions
(defmacro doit2 ()
  `(progn
     ,@(loop for l in *c-types* collect
             `(fli:define-c-typedef ,(first l) ,(second l)))))
  
;;; #NOTE: Se deben crear los bindings aqui sino es un problemita
;(doit2)
|#

(PROGN
  (FLI:DEFINE-C-TYPEDEF OCL::STR (:REFERENCE-PASS :EF-MB-STRING))
  (FLI:DEFINE-C-TYPEDEF OCL::INT (:SIGNED-INTEGER-TYPE 32))
  (FLI:DEFINE-C-TYPEDEF OCL::UINT (:UNSIGNED-INTEGER-TYPE 32))
  (FLI:DEFINE-C-TYPEDEF OCL::MASK (:UNSIGNED-INTEGER-TYPE 64))
  (FLI:DEFINE-C-TYPEDEF OCL::BOOL OCL::UINT)
  (FLI:DEFINE-C-TYPEDEF OCL::PTR :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL::LISP-ARRAY :LISP-ARRAY)
  (FLI:DEFINE-C-TYPEDEF OCL::PARAM :SIGNED)
  (FLI:DEFINE-C-TYPEDEF OCL::SIZE-T :UNSIGNED)
  (FLI:DEFINE-C-TYPEDEF OCL::CALLBACK :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:PLATFORM :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:DEVICE :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:CONTEXT :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:QUEUE :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:BUFFER :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:PROGRAM :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:KERNEL :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:EVENT :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:IMAGE-FORMAT :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL::VOID :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL:SAMPLER :POINTER)
  (FLI:DEFINE-C-TYPEDEF OCL::PROGRAM-BUILD-INFO :POINTER))


(defun platform-count ()
  "Returns the number of OpenCL platforms."
  (fli:with-dynamic-foreign-objects ((count UINT))
    (check (|clGetPlatformIDs| 0 nil count)
           (fli:dereference count))))

(defun platforms ()
  "Returns a list with available OpenCL platforms."
  (let ((count (platform-count)))
    (fli:with-dynamic-foreign-objects ((platforms PLATFORM :nelems count))
      (check (|clGetPlatformIDs| count platforms nil)
        (loop for n :below count collect 
              (make-instance 'ocl:platform :object (fli:dereference platforms :index n)))))))

(defun device-count (platform)
  "Returns the number of devices for the given platform."
  (fli:with-dynamic-foreign-objects ((count UINT))
    (check (|clGetDeviceIDs| (object platform) #xFFFFFFFF 0 nil count)
           (fli:dereference count))))

(defun devices (platform)
  "Returns a list with the devices of the given platform."
  (let ((count (device-count platform)))
    (fli:with-dynamic-foreign-objects ((devices DEVICE :nelems count))
      (check (|clGetDeviceIDs| (object platform) #xFFFFFFFFF count devices nil)
             (loop for n :below count collect 
                   (make-instance 'ocl:device :object (fli:dereference devices :index n)))))))

(defun create-context (devices)
  "Creates an OpenCL context from the list of the devices."
  (let ((device-count (length devices))
        (devices (mapcar (lambda (device) (object device)) devices)))
    (fli:with-dynamic-foreign-objects
        ((devices DEVICE :nelems device-count :initial-contents devices)
         (error INT))
      (check2 (|clCreateContext| nil device-count devices nil nil error)
              (fli:dereference error)))))

(defun create-queue (context device &optional (queue-flags 0 queue-flags-supplied-p))
  "Creates an OpenCL queue in the given context."
  (fli:with-dynamic-foreign-objects ((error INT))
    (values (|clCreateCommandQueue| context (object device) queue-flags error)
            (fli:dereference error))))

(defun create-buffer-helper (context size &optional (flags 0) (host-ptr nil))
  (fli:with-dynamic-foreign-objects ((error INT))
    (check2 (|clCreateBuffer| context flags size host-ptr error)
            (fli:dereference error))))

(defun create-read-buffer (context size &optional (host-ptr nil))
  (create-buffer-helper context size 0 host-ptr))

(defun create-write-buffer (context size &optional (host-ptr nil))
  (create-buffer-helper context size 0 host-ptr))

(defun dalloc (type initial-contents)
  (let ((length (length initial-contents)))
    (if (zerop length)
        nil
      (fli:allocate-dynamic-foreign-object :type type :initial-contents initial-contents :nelems length))))

(defun enqueue-read-buffer (queue buffer lisp-array size &key (blocking 1) (waiting-events nil) (offset 0))
  (let ((waiting-count (length waiting-events)))
    (fli:with-dynamic-foreign-objects ((event EVENT))
      (check (|clEnqueueReadBuffer| queue buffer blocking offset size lisp-array waiting-count (dalloc EVENT waiting-events) event)
             (fli:dereference event)))))

(defun enqueue-copy-buffer (queue src-buffer dst-buffer size &key (waiting-events nil) (src-offset 0) (dst-offset 0))
  (let ((waiting-count (length waiting-events)))
    (fli:with-dynamic-foreign-objects ((event EVENT))
      (check (|clEnqueueCopyBuffer| queue src-buffer dst-buffer src-offset dst-offset size waiting-count (dalloc EVENT waiting-events) event)
             (fli:dereference event)))))

;; #TODO: Optimize these three functions next
(defun get-parameter-id (id)
  (third (find-if (lambda (o) (eq (second o) id)) +parameters+)))

(defun get-parameter-type (id)
  (fourth (find-if (lambda (o) (eq (second o) id)) +parameters+)))

(defun array-to-string (array)
  (let ((vector (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (result array))
    (dotimes (i (length result))
      (let ((char-code (code-char (aref result i))))
        (vector-push-extend char-code vector)))
    (subseq vector 0 (array-first-index vector #\null))))

(defun array-first-index (array element)
  (dotimes (i (length array))
    (when (equal (aref array i) element)
      (return i))))

(defun get-param-info (parameter-name object)
  (fli:with-dynamic-foreign-objects ((size :unsigned 0))
    (let* ((object (object object))
           (param (get-parameter-id parameter-name))
           (error (|clGetDeviceInfo| object param 0 nil size)))
      (if (zerop error)
          (let ((size (fli:dereference size)))
            (fli:with-dynamic-foreign-objects ((data :byte :nelems size))
              (let ((error (|clGetDeviceInfo| object param size data nil)))
                (if (zerop error)
                    (let ((array (make-array size :element-type '(unsigned-byte 8))))
                      (dotimes (n size) 
                        (setf (aref array n) (fli:dereference data :index n)))
                      (values (convert-to-type array (get-parameter-type parameter-name)) error))
                  (values nil error)))))
        (values nil error)))))

(defun convert-to-type (array type)
  (let ((value 0))
    (ecase type
      ('STRING (array-to-string array))
      ('BOOL (not (equal (aref array 0) 0)))
      ('UINT (progn (dotimes (i 4) (incf value (* (expt 256 i) (aref array i)))) value))
      ('ULONG (progn (dotimes (i 4) (incf value (* (expt 256 i) (aref array i)))) value))
      ('SIZE-T (progn (dotimes (i 4) (incf value (* (expt 256 i) (aref array i)))) value)))))

(defmethod initialize-instance :after ((context context) &rest initargs &key &allow-other-keys)
  (with-slots (object devices) context
    (make-context devices)))

;; Core program
(defun convert-strings-to-foreign-array (strings &key (allocation :static))
  (let* ((count (length strings))
         (array (fli:allocate-foreign-object 
                 :type '(:pointer (:unsigned :char))
                 :nelems (1+ count)
                 :initial-element nil)))
    (loop for index from 0
          for string in strings
          do (setf (fli:dereference array :index index)
                   (fli:convert-to-foreign-string
                    string
                    :external-format :utf-8)))
    array))

(defun build-program-from-source (context program-source &optional (compile-options ""))
  (fli:with-dynamic-foreign-objects 
    ((size :unsigned (length program-source))
     (error ocl::int))
  (let* ((array (convert-strings-to-foreign-array (list program-source) :allocation :dynamic))
         (program (ocl::|clCreateProgramWithSource| context 1 array size error)))
    (check-cl (fli:dereference error))
    (build-program program compile-options)
    program)))

(defun build-program (program &optional (compile-options ""))
  "Build <program> for <devices>."
  (let ((error (|clBuildProgram| program 0 nil compile-options nil nil)))
    (check-cl error)))

(defun set-kernel-argument (kernel argument-index argument-size argument-value)
  (let ((error (|clSetKernelArg| kernel argument-index argument-size argument-value)))
    (check-cl error)))

(defun create-kernel (program name)
  (fli:with-dynamic-foreign-objects
      ((error ocl::int))
    (let ((kernel (|clCreateKernel| program name error)))
      (check-cl (fli:dereference error))
      kernel)))

(defun convert-float-foreign-array (floats)
  (fli:allocate-foreign-object
   :type (list :foreign-array :float floats)))

(defun create-buffer (context size flags &optional (host-ptr nil))
  (fli:with-dynamic-foreign-objects
      ((error ocl::int))
    (let ((buffer (|clCreateBuffer| context flags size nil error)))
      (check-cl (fli:dereference error))
      buffer)))

(defmacro setf-buffer-argument (context var kernel index size flags contents)
  `(progn 
     (setf ,var (create-buffer-helper ,context ,size ,flags ,contents))
     (fli:with-dynamic-foreign-objects
         ((buffer ocl:buffer))
       (setf (fli:dereference buffer) ,var)
       (set-kernel-argument ,kernel ,index 4 buffer))))

(defmethod enqueue-kernel (kernel queue global-work-size local-work-size)
  (fli:with-dynamic-foreign-objects
      ((global-size ocl::size-t)
       (local-size ocl::size-t)) 
    (setf (fli:dereference global-size) global-work-size
          (fli:dereference local-size) local-work-size)
    (check-cl (|clEnqueueNDRangeKernel| queue kernel 1 0 global-size local-size 0 nil nil))))


#|
(flet ((create-buffer-helper (context size &optional (flags 0) (host-ptr nil))
         (fli:with-dynamic-foreign-objects ((error INT))
           (check2 (|clCreateBuffer| context flags size host-ptr error)
                   (fli:dereference error)))))
  (defun create-read-buffer (context size &optional (host-ptr nil))
    (create-buffer-helper context size 2 host-ptr))
  (defun create-write-buffer (context size &optional (host-ptr nil))
    (create-buffer-helper context size 1 host-ptr))
  (defun create-buffer (context size &optional (host-ptr nil))
    (create-buffer-helper context size 0 host-ptr)))

#define CL_MEM_READ_WRITE                           (1 << 0)
#define CL_MEM_WRITE_ONLY                           (1 << 1)
#define CL_MEM_READ_ONLY                            (1 << 2)
#define CL_MEM_USE_HOST_PTR                         (1 << 3)
#define CL_MEM_ALLOC_HOST_PTR                       (1 << 4)
#define CL_MEM_COPY_HOST_PTR                        (1 << 5)

(defmethod info-function ((platform platform)) #'cl-get-platform-info)
(defmethod info-function ((device device)) #'cl-get-device-info)
(defmethod info-function ((context context)) #'cl-get-context-info)

(defgeneric info (param object &optional object2))

(defmethod info (param (platform platform) &optional object2)
  (let ((platform (slot-value platform 'object)))
    (fli:with-dynamic-foreign-objects ((count :unsigned))
      (let ((error1 (cl-get-platform-info platform param 0 nil count)))
        (let ((count (fli:dereference count)))
          (fli:with-dynamic-foreign-objects ((param-value :byte :nelems count))
            (let ((error2 (cl-get-platform-info platform param count param-value nil)))
              (values param-value count error1 error2))))))))

(fli:define-foreign-callable ("notify" :result-type :void)
    ((error-info   (:reference-return :ef-mb-string))
     (private-info :pointer)
     (private-size :int)
     (user-data    :pointer))
  (format t "OpenCL: error    = ~A~&" error-info)
  (format t "OpenCL: private  = ~A ~A~&" private-size private-info)
  (format t "OpenCL: userdata = ~A~&" user-data))

(defmacro make-buffer-helper (buffer-flags)
  `(fli:with-dynamic-foreign-objects ((error s32))
     (values (cl-create-buffer context ,buffer-flags size nil error)
             (fli:dereference error))))
(defun make-buffer       (context size) (make-buffer-helper 0))
(defun make-read-buffer  (context size) (make-buffer-helper 2))
(defun make-write-buffer (context size) (make-buffer-helper 1))
|#

(defun release-queue (queue)
  ;(ocl::|clReleaseQueue| queue)
  (declare (ignore queue))
  nil)

(defun release-context (context)
  (ocl::|clReleaseContext| context))

#|
;; Create all bindings
(defmacro doit ()
  `(progn
     ,@(loop for l in ocl::*c-types* collect
             `(fli:define-c-typedef ,(first l) ,(second l)))
     ,@(loop for l in +c-functions+ collect
             `(fli:define-foreign-function (,(first l) ,(format nil "~A" (first l)))
                  ,(loop for p in (third l)
                         collect `(,(second p)
                                   ,(if (atom (first p))
                                        (first p)
                                      `(:pointer ,(first (first p))))))
                :result-type ,(second l)
                :module "OpenCL"
                :calling-convention #+win32 :stdcall #-win32 :cdecl))))
|#

(PROGN
   (FLI:DEFINE-C-TYPEDEF OCL::STR (:REFERENCE-PASS :EF-MB-STRING))
   (FLI:DEFINE-C-TYPEDEF OCL::INT (:SIGNED-INTEGER-TYPE 32))
   (FLI:DEFINE-C-TYPEDEF OCL::UINT (:UNSIGNED-INTEGER-TYPE 32))
   (FLI:DEFINE-C-TYPEDEF OCL::MASK (:UNSIGNED-INTEGER-TYPE 64))
   (FLI:DEFINE-C-TYPEDEF OCL::BOOL OCL::UINT)
   (FLI:DEFINE-C-TYPEDEF OCL::PTR :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL::LISP-ARRAY :LISP-ARRAY)
   (FLI:DEFINE-C-TYPEDEF OCL::PARAM :SIGNED)
   (FLI:DEFINE-C-TYPEDEF OCL::SIZE-T :UNSIGNED)
   (FLI:DEFINE-C-TYPEDEF OCL::CALLBACK :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:PLATFORM :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:DEVICE :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:CONTEXT :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:QUEUE :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:BUFFER :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:PROGRAM :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:KERNEL :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:EVENT :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:IMAGE-FORMAT :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL::VOID :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL:SAMPLER :POINTER)
   (FLI:DEFINE-C-TYPEDEF OCL::PROGRAM-BUILD-INFO :POINTER)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetPlatformIDs| "clGetPlatformIDs")
                                ((OCL::NUM-ENTRIES OCL::UINT)
                                 (OCL:PLATFORMS (:POINTER OCL:PLATFORM))
                                 (OCL::NUM-PLATFORMS (:POINTER OCL::UINT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetPlatformInfo| "clGetPlatformInfo")
                                ((OCL:PLATFORM OCL:PLATFORM)
                                 (OCL::PLATFORM-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetDeviceIDs| "clGetDeviceIDs")
                                ((OCL:PLATFORM OCL:PLATFORM)
                                 (OCL::DEVICE-TYPE OCL::MASK)
                                 (OCL::NUM-ENTRIES OCL::UINT)
                                 (OCL:DEVICES (:POINTER OCL:DEVICE))
                                 (OCL::NUM-DEVICES (:POINTER OCL::UINT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetDeviceInfo| "clGetDeviceInfo")
                                ((OCL:DEVICE OCL:DEVICE)
                                 (OCL::DEVICE-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateContext| "clCreateContext")
                                ((OCL::CONTEXT-PROPERTIES OCL::PTR)
                                 (OCL::NUM-DEVICES OCL::UINT)
                                 (OCL:DEVICES (:POINTER OCL:DEVICE))
                                 (OCL::NOTIFY OCL::CALLBACK)
                                 (OCL::USER-DATA OCL::PTR)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:CONTEXT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateContextFromType| "clCreateContextFromType")
                                ((OCL::CONTEXT-PROPERTIES OCL::PTR)
                                 (OCL::DEVICE-TYPE OCL::MASK)
                                 (OCL::NOTIFY OCL::CALLBACK)
                                 (OCL::USER-DATA OCL::PTR)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:CONTEXT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clRetainContext| "clRetainContext")
                                ((OCL:CONTEXT OCL:CONTEXT))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clReleaseContext| "clReleaseContext")
                                ((OCL:CONTEXT OCL:CONTEXT))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetContextInfo| "clGetContextInfo")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::CONTEXT-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateCommandQueue| "clCreateCommandQueue")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL:DEVICE OCL:DEVICE)
                                 (OCL::COMMAND-QUEUE-PROPERTIES OCL::MASK)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:QUEUE
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clRetainQueue| "clRetainQueue")
                                ((OCL:QUEUE OCL:QUEUE))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clReleaseQueue| "clReleaseQueue")
                                ((OCL:QUEUE OCL:QUEUE))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetCommandQueueInfo| "clGetCommandQueueInfo")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL::COMMAND-QUEUE-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clSetCommandQueueProperty| "clSetCommandQueueProperty")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL::COMMAND-QUEUE-PROPERTIES OCL::MASK)
                                 (OCL::ENABLED OCL::BOOL)
                                 (OCL::COMMAND-QUEUE-PROPERTIES-RETURNED (:POINTER OCL::MASK)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateBuffer| "clCreateBuffer")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::MEMORY-FLAGS OCL::MASK)
                                 (OCL::MEMORY-SIZE OCL::SIZE-T)
                                 (OCL::HOST-ARRAY OCL::LISP-ARRAY)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:BUFFER
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateImage2D| "clCreateImage2D")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::MEMORY-FLAGS OCL::MASK)
                                 (FORMAT (:POINTER OCL:IMAGE-FORMAT))
                                 (OCL::WIDTH OCL::SIZE-T)
                                 (OCL::HEIGHT OCL::SIZE-T)
                                 (OCL::ROW-PITCH OCL::SIZE-T)
                                 (OCL::HOST-ARRAY OCL::LISP-ARRAY)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:BUFFER
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateImage3D| "clCreateImage3D")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::MEMORY-FLAGS OCL::MASK)
                                 (FORMAT (:POINTER OCL:IMAGE-FORMAT))
                                 (OCL::WIDTH OCL::SIZE-T)
                                 (OCL::HEIGHT OCL::SIZE-T)
                                 (OCL::DEPTH OCL::SIZE-T)
                                 (OCL::ROW-PITCH OCL::SIZE-T)
                                 (OCL::SLICE-PITCH OCL::SIZE-T)
                                 (OCL::HOST-ARRAY OCL::LISP-ARRAY)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:BUFFER
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clRetainMemObject| "clRetainMemObject")
                                ((OCL:BUFFER OCL:BUFFER))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clReleaseMemObject| "clReleaseMemObject")
                                ((OCL::BUFFFER OCL:BUFFER))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetSupportedImageFormats| "clGetSupportedImageFormats")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::MEMORY-FLAGS OCL::MASK)
                                 (OCL::IMAGE-TYPE OCL::PARAM)
                                 (OCL::NUM-ENTRIES OCL::UINT)
                                 (OCL::IMAGE-FORMATS (:POINTER OCL:IMAGE-FORMAT))
                                 (OCL::NUM-IMAGE-FORMATS-RETURNED (:POINTER OCL::UINT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetMemObjectInfo| "clGetMemObjectInfo")
                                ((OCL:BUFFER OCL:BUFFER)
                                 (OCL::MEMORY-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetImageInfo| "clGetImageInfo")
                                ((OCL:BUFFER OCL:BUFFER)
                                 (OCL::IMAGE-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateSampler| "clCreateSampler")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::NORMALIZED-COORDS OCL::BOOL)
                                 (OCL::ADDRESSING-MODE OCL::UINT)
                                 (OCL::FILTER-MODE OCL::UINT)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:SAMPLER
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clRetainSampler| "clRetainSampler")
                                ((OCL:SAMPLER OCL:SAMPLER))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clReleaseSampler| "clReleaseSampler")
                                ((OCL:SAMPLER OCL:SAMPLER))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetSamplerInfo| "clGetSamplerInfo")
                                ((OCL:SAMPLER OCL:SAMPLER)
                                 (OCL::SAMPLER-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateProgramWithSource| "clCreateProgramWithSource")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::STRING-COUNT OCL::UINT)
                                 (OCL::STRINGS OCL::PTR)
                                 (OCL::STRING-LENGTHS (:POINTER OCL::SIZE-T))
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:PROGRAM
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateProgramWithBinary| "clCreateProgramWithBinary")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL:DEVICE-COUNT OCL::UINT)
                                 (OCL:DEVICES (:POINTER OCL:DEVICE))
                                 (OCL::BINARY-LENGTHS (:POINTER OCL::SIZE-T))
                                 (OCL::BINARIES OCL::PTR)
                                 (OCL::BINARY-STATUS (:POINTER OCL::INT))
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:PROGRAM
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clRetainProgram| "clRetainProgram")
                                ((OCL:PROGRAM OCL:PROGRAM))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clReleaseProgram| "clReleaseProgram")
                                ((OCL:PROGRAM OCL:PROGRAM))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clBuildProgram| "clBuildProgram")
                                ((OCL:PROGRAM OCL:PROGRAM)
                                 (OCL:DEVICE-COUNT OCL::UINT)
                                 (OCL:DEVICES (:POINTER OCL:DEVICE))
                                 (OCL::OPTIONS OCL::STR)
                                 (OCL::NOTIFY OCL::CALLBACK)
                                 (OCL::USER-DATA OCL::PTR))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clUnloadCompiler| "clUnloadCompiler")
                                NIL
                                :RESULT-TYPE
                                OCL::VOID
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetProgramInfo| "clGetProgramInfo")
                                ((OCL:PROGRAM OCL:PROGRAM)
                                 (OCL::PROGRAM-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateKernel| "clCreateKernel")
                                ((OCL:PROGRAM OCL:PROGRAM) (OCL::KERNEL-NAME OCL::STR) (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:KERNEL
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateKernelsInProgram| "clCreateKernelsInProgram")
                                ((OCL:PROGRAM OCL:PROGRAM)
                                 (OCL::KERNEL-COUNT OCL::UINT)
                                 (OCL::KERNELS (:POINTER OCL:KERNEL))
                                 (OCL::KERNEL-COUNT-RETURNED (:POINTER OCL::UINT)))
                                :RESULT-TYPE
                                OCL:KERNEL
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clRetainKernel| "clRetainKernel")
                                ((OCL:KERNEL OCL:KERNEL))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clReleaseKernel| "clReleaseKernel")
                                ((OCL:KERNEL OCL:KERNEL))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clSetKernelArg| "clSetKernelArg")
                                ((OCL:KERNEL OCL:KERNEL)
                                 (OCL::ARGUMENT-INDEX OCL::UINT)
                                 (OCL::ARGUMENT-SIZE OCL::SIZE-T)
                                 (OCL::ARGUMENT-VALUE OCL::PTR))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetKernelInfo| "clGetKernelInfo")
                                ((OCL:KERNEL OCL:KERNEL)
                                 (OCL::KERNEL-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetKernelWorkGroupInfo| "clGetKernelWorkGroupInfo")
                                ((OCL:KERNEL OCL:KERNEL)
                                 (OCL::DEVICEE OCL:DEVICE)
                                 (OCL::KERNEL-WORKGROUP-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clWaitForEvents| "clWaitForEvents")
                                ((OCL::EVENT-COUNT OCL::UINT) (OCL::EVENTS (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetEventInfo| "clGetEventInfo")
                                ((OCL:EVENT OCL:EVENT) (OCL::EVENT-INFO OCL::PARAM))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clRetainEvent| "clRetainEvent")
                                ((OCL:EVENT OCL:EVENT))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clReleaseEvent| "clReleaseEvent")
                                ((OCL:EVENT OCL:EVENT))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetEventProfilingInfo| "clGetEventProfilingInfo")
                                ((OCL:EVENT OCL:EVENT)
                                 (OCL::PROFILING-INFO OCL::PARAM)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clFlush| "clFlush")
                                ((OCL:QUEUE OCL:QUEUE))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clFinish| "clFinish")
                                ((OCL:QUEUE OCL:QUEUE))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueReadBuffer| "clEnqueueReadBuffer")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL:BUFFER OCL:BUFFER)
                                 (OCL::BLOCKING-READ OCL::BOOL)
                                 (OCL::OFFSET OCL::SIZE-T)
                                 (OCL::CB OCL::SIZE-T)
                                 (ARRAY OCL::LISP-ARRAY)
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueWriteBuffer| "clEnqueueWriteBuffer")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL:BUFFER OCL:BUFFER)
                                 (OCL::BLOCKING-WRITE OCL::BOOL)
                                 (OCL::OFFSET OCL::SIZE-T)
                                 (OCL::CB OCL::SIZE-T)
                                 (ARRAY OCL::LISP-ARRAY)
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueCopyBuffer| "clEnqueueCopyBuffer")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL::SRC-BUFFER OCL:BUFFER)
                                 (OCL::DST-BUFFER OCL:BUFFER)
                                 (OCL::SRC-OFFSET OCL::SIZE-T)
                                 (OCL::DST-OFFSET OCL::SIZE-T)
                                 (OCL::SIZE OCL::SIZE-T)
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueReadImage| "clEnqueueReadImage")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL:BUFFER OCL:BUFFER)
                                 (OCL::BLOCKING-READ OCL::BOOL)
                                 (OCL::ORIGIN (:POINTER OCL::SIZE-T))
                                 (OCL::REGION (:POINTER OCL::SIZE-T))
                                 (OCL::ROW-PITCH OCL::SIZE-T)
                                 (OCL::SLICE-PITCH OCL::SIZE-T)
                                 (ARRAY OCL::LISP-ARRAY)
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueWriteImage| "clEnqueueWriteImage")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL:BUFFER OCL:BUFFER)
                                 (OCL::BLOCKING-WRITE OCL::BOOL)
                                 (OCL::ORIGIN (:POINTER OCL::SIZE-T))
                                 (OCL::REGION (:POINTER OCL::SIZE-T))
                                 (OCL::ROW-PITCH OCL::SIZE-T)
                                 (OCL::SLICE-PITCH OCL::SIZE-T)
                                 (ARRAY OCL::LISP-ARRAY)
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueCopyImage| "clEnqueueCopyImage")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL::SRC-BUFFER OCL:BUFFER)
                                 (OCL::DST-BUFFER OCL:BUFFER)
                                 (OCL::SRC-ORIGIN (:POINTER OCL::SIZE-T))
                                 (OCL::REGION (:POINTER OCL::SIZE-T))
                                 (OCL::DST-OFFSET OCL::SIZE-T)
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueCopyImageToBuffer| "clEnqueueCopyImageToBuffer")
                                NIL
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueCopyBufferToImage| "clEnqueueCopyBufferToImage")
                                NIL
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueMapBuffer| "clEnqueueMapBuffer")
                                NIL
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueMapImage| "clEnqueueMapImage")
                                NIL
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueUnmapMemObject| "clEnqueueUnmapMemObject")
                                NIL
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueTask| "clEnqueueTask")
                                NIL
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueNativeKernel| "clEnqueueNativeKernel")
                                NIL
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueMarker| "clEnqueueMarker")
                                ((OCL:QUEUE OCL:QUEUE) (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueWaitForEvents| "clEnqueueWaitForEvents")
                                ((OCL:QUEUE OCL:QUEUE) (OCL::EVENT-COUNT OCL::UINT) (OCL::EVENTS (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueBarrier| "clEnqueueBarrier")
                                ((OCL:QUEUE OCL:QUEUE))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetExtensionFunctionAddress| "clGetExtensionFunctionAddress")
                                ((OCL::FUNCTION-NAME OCL::STR))
                                :RESULT-TYPE
                                OCL::PTR
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateFromGLBuffer| "clCreateFromGLBuffer")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::MEMORY-FLAGS OCL::MASK)
                                 (OCL::BUFFER-OBJECT OCL::SIZE-T)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:BUFFER
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateFromGLTexture2D| "clCreateFromGLTexture2D")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::MEMORY-FLAGS OCL::MASK)
                                 (OCL::TEXTURE-TARGET OCL::SIZE-T)
                                 (OCL::MIP-LEVEL OCL::SIZE-T)
                                 (OCL::TEXTURE-OBJECT OCL::SIZE-T)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:BUFFER
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateFromGLTexture3D| "clCreateFromGLTexture3D")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::MEMORY-FLAGS OCL::MASK)
                                 (OCL::TEXTURE-TARGET OCL::SIZE-T)
                                 (OCL::MIP-LEVEL OCL::SIZE-T)
                                 (OCL::TEXTURE-OBJECT OCL::SIZE-T)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:BUFFER
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateFromGLRenderbuffer| "clCreateFromGLRenderbuffer")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (OCL::MEMORY-FLAGS OCL::MASK)
                                 (OCL::RENDER-BUFFER OCL::SIZE-T)
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:BUFFER
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetGLObjectInfo| "clGetGLObjectInfo")
                                ((OCL:BUFFER OCL:BUFFER)
                                 (OCL::GL-OBJECT-TYPE (:POINTER OCL::UINT))
                                 (OCL::GL-OBJECT-NAME (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetGLTextureInfo| "clGetGLTextureInfo")
                                ((OCL:BUFFER OCL:BUFFER)
                                 (OCL::GL-TEXTURE-INFO OCL::UINT)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueAcquireGLObjects| "clEnqueueAcquireGLObjects")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL::BUFFER-COUNT OCL::UINT)
                                 (OCL::BUFFERS (:POINTER OCL:BUFFER))
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clReleaseAcquireGLObjects| "clReleaseAcquireGLObjects")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL::BUFFER-COUNT OCL::UINT)
                                 (OCL::BUFFERS (:POINTER OCL:BUFFER))
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clCreateProgramWithSource| "clCreateProgramWithSource")
                                ((OCL:CONTEXT OCL:CONTEXT)
                                 (COUNT OCL::UINT)
                                 (OCL::STRINGS (:POINTER :POINTER))
                                 (OCL::PROGRAM-LENGHTS (:POINTER OCL::SIZE-T))
                                 (OCL::ERROR-RETURNED (:POINTER OCL::INT)))
                                :RESULT-TYPE
                                OCL:PROGRAM
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clGetProgramBuildInfo| "clGetProgramBuildInfo")
                                ((OCL:PROGRAM OCL:PROGRAM)
                                 (OCL:DEVICE OCL:DEVICE)
                                 (OCL::BUILD-INFO OCL::PROGRAM-BUILD-INFO)
                                 (OCL::PARAM-VALUE-SIZE OCL::SIZE-T)
                                 (OCL::PARAM-VALUE OCL::PTR)
                                 (OCL::PARAM-VALUE-SIZE-RETURNED (:POINTER OCL::SIZE-T)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL)
   (FLI:DEFINE-FOREIGN-FUNCTION (OCL::|clEnqueueNDRangeKernel| "clEnqueueNDRangeKernel")
                                ((OCL:QUEUE OCL:QUEUE)
                                 (OCL:KERNEL OCL:KERNEL)
                                 (OCL::WORK-DIM OCL::UINT)
                                 (OCL::GLOBAL-WORK-OFFSET OCL::SIZE-T)
                                 (OCL::GLOBAL-WORK-SIZE (:POINTER OCL::SIZE-T))
                                 (OCL::LOCAL-WORK-SIZE (:POINTER OCL::SIZE-T))
                                 (OCL::WAITING-EVENT-COUNT OCL::UINT)
                                 (OCL::WAITING-EVENTS (:POINTER OCL:EVENT))
                                 (OCL::EVENT-RETURNED (:POINTER OCL:EVENT)))
                                :RESULT-TYPE
                                OCL::INT
                                :MODULE
                                "OpenCL"
                                :CALLING-CONVENTION
                                :STDCALL))

