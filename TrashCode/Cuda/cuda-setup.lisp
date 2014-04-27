
;; DEFINE ATTRIBUTE-CONSTANTS
(defvar CU-DEVICE-ATTRIBUTE-MAX-THREADS-PER-BLOCK 1)
(defvar CU-DEVICE-ATTRIBUTE-MAX-BLOCK-DIM-X 2)
(defvar CU-DEVICE-ATTRIBUTE-MAX-BLOCK-DIM-Y 3)
(defvar CU-DEVICE-ATTRIBUTE-MAX-BLOCK-DIM-Z 4)
(defvar CU-DEVICE-ATTRIBUTE-MAX-GRID-DIM-X 5)
(defvar CU-DEVICE-ATTRIBUTE-MAX-GRID-DIM-Y 6)
(defvar CU-DEVICE-ATTRIBUTE-MAX-GRID-DIM-Z 7)
(defvar CU-DEVICE-ATTRIBUTE-MAX-SHARED-MEMORY-PER-BLOCK 8)
(defvar CU-DEVICE-ATTRIBUTE-SHARED-MEMORY-PER-BLOCK 8)
(defvar CU-DEVICE-ATTRIBUTE-TOTAL-CONSTANT-MEMORY 9)
(defvar CU-DEVICE-ATTRIBUTE-WARP-SIZE 10)
(defvar CU-DEVICE-ATTRIBUTE-MAX-PITCH 11)
(defvar CU-DEVICE-ATTRIBUTE-MAX-REGISTERS-PER-BLOCK 12)
(defvar CU-DEVICE-ATTRIBUTE-REGISTERS-PER-BLOCK 12)
(defvar CU-DEVICE-ATTRIBUTE-CLOCK-RATE 13)
(defvar CU-DEVICE-ATTRIBUTE-TEXTURE-ALIGNMENT 14)
(defvar CU-DEVICE-ATTRIBUTE-GPU-OVERLAP 15)
(defvar CU-DEVICE-ATTRIBUTE-MULTIPROCESSOR-COUNT 16)
(defvar CU-DEVICE-ATTRIBUTE-KERNEL-EXEC-TIMEOUT 17)
(defvar CU-DEVICE-ATTRIBUTE-INTEGRATED 18)
(defvar CU-DEVICE-ATTRIBUTE-CAN-MAP-HOST-MEMORY 19)
(defvar CU-DEVICE-ATTRIBUTE-COMPUTE-MODE 20)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE1D-WIDTH 21)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE2D-WIDTH 22)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE2D-HEIGHT 23)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE3D-WIDTH 24)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE3D-HEIGHT 25)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE3D-DEPTH 26)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE2D-ARRAY-WIDTH 27)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE2D-ARRAY-HEIGHT 28)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE2D-ARRAY-NUMSLICES 29)
(defvar CU-DEVICE-ATTRIBUTE-SURFACE-ALIGNMENT 30)
(defvar CU-DEVICE-ATTRIBUTE-CONCURRENT-KERNELS 31)
(defvar CU-DEVICE-ATTRIBUTE-ECC-ENABLED 32)
(defvar CU-DEVICE-ATTRIBUTE-PCI-BUS-ID 33)
(defvar CU-DEVICE-ATTRIBUTE-PCI-DEVICE-ID 34)
(defvar CU-DEVICE-ATTRIBUTE-TCC-DRIVER 35)
;; CUDA-API-VERSION >= 4000
(defvar CU-DEVICE-ATTRIBUTE-MEMORY-CLOCK-RATE 36)
(defvar CU-DEVICE-ATTRIBUTE-GLOBAL-MEMORY-BUS-WIDTH 37)
(defvar CU-DEVICE-ATTRIBUTE-L2-CACHE-SIZE 38)
(defvar CU-DEVICE-ATTRIBUTE-MAX-THREADS-PER-MULTIPROCESSOR 39)
(defvar CU-DEVICE-ATTRIBUTE-ASYNC-ENGINE-COUNT 40)
(defvar CU-DEVICE-ATTRIBUTE-UNIFIED-ADDRESSING 41)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE1D-LAYERED-WIDTH 42)
(defvar CU-DEVICE-ATTRIBUTE-MAXIMUM-TEXTURE1D-LAYERED-LAYERS 43)

;; DEFINE CONTEXT MODE CONSTANTS
(defvar CU-CTX-SCHED-AUTO 0)
(defvar CU-CTX-SCHED-SPIN 1)
(defvar CU-CTX-SCHED-YIELD 2)
(defvar CU-CTX-SCHED-BLOCKING-SYNC 4)
(defvar CU-CTX-BLOCKING-SYNC 4)
(defvar CU-CTX-MAP-HOST 8)
(defvar CU-CTX-LMEM-RESIZE-TO-MAX 16)
;; CUDA-API-VERSION < 4000
(defvar CU-CTX-SCHED-MASK 3) 
(defvar CU-CTX-FLAGS-MASK 31)
(defvar CU-CTX-SCHED-MASK 7) 
(defvar CU-CTX-PRIMARY 32) 
(defvar CU-CTX-FLAGS-MASK 63)

;; DEFINE ERROR CONSTANTS
(defvar CUDA-SUCCESS 0) 
(defvar CUDA-ERROR-INVALID-VALUE 1) 
(defvar CUDA-ERROR-OUT-OF-MEMORY 2) 
(defvar CUDA-ERROR-NOT-INITIALIZED 3) 
(defvar CUDA-ERROR-DEINITIALIZED 4) 
(defvar CUDA-ERROR-NO-DEVICE 100) 
(defvar CUDA-ERROR-INVALID-DEVICE 101) 
(defvar CUDA-ERROR-INVALID-IMAGE 200) 
(defvar CUDA-ERROR-INVALID-CONTEXT 201) 
(defvar CUDA-ERROR-CONTEXT-ALREADY-CURRENT 202) 
(defvar CUDA-ERROR-MAP-FAILED 205) 
(defvar CUDA-ERROR-UNMAP-FAILED 206) 
(defvar CUDA-ERROR-ARRAY-IS-MAPPED 207)  
(defvar CUDA-ERROR-ALREADY-MAPPED 208)  
(defvar CUDA-ERROR-NO-BINARY-FOR-GPU 209) 
(defvar CUDA-ERROR-ALREADY-ACQUIRED 210) 
(defvar CUDA-ERROR-NOT-MAPPED 211) 
(defvar CUDA-ERROR-NOT-MAPPED-AS-ARRAY 212) 
(defvar CUDA-ERROR-NOT-MAPPED-AS-POINTER 211) 
(defvar CUDA-ERROR-ECC-UNCORRECTABLE 214) 
(defvar CUDA-ERROR-UNSUPPORTED-LIMIT 215) 
(defvar CUDA-ERROR-CONTEXT-ALREADY-IN-USE 216) 
(defvar CUDA-ERROR-INVALID-SOURCE 300) 
(defvar CUDA-ERROR-FILE-NOT-FOUND 301) 
(defvar CUDA-ERROR-SHARED-OBJECT-SYMBOL-NOT-FOUND 302) 
(defvar CUDA-ERROR-SHARED-OBJECT-INIT-FAILED 303) 
(defvar CUDA-ERROR-OPERATING-SYSTEM 304) 
(defvar CUDA-ERROR-INVALID-HANDLE 400) 
(defvar CUDA-ERROR-NOT-FOUND 500) 
(defvar CUDA-ERROR-NOT-READY 600) 
(defvar CUDA-ERROR-LAUNCH-FAILED 700) 
(defvar CUDA-ERROR-LAUNCH-OUT-OF-RESOURCES 701) 
(defvar CUDA-ERROR-LAUNCH-TIMEOUT 702) 
(defvar CUDA-ERROR-LAUNCH-INCOMPATIBLE-TEXTURING 703)
(defvar CUDA-ERROR-PEER-ACCESS-ALREADY-ENABLED 704)
(defvar CUDA-ERROR-PEER-ACCESS-NOT-ENABLED 705)
(defvar CUDA-ERROR-PEER-MEMORY-ALREADY-REGISTERED 706)
(defvar CUDA-ERROR-PEER-MEMORY-NOT-REGISTERED 707)
(defvar CUDA-ERROR-PRIMARY-CONTEXT-ACTIVE 708)
(defvar CUDA-ERROR-CONTEXT-IS-DESTROYED 709)
(defvar CUDA-ERROR-UNKNOWN 999)

;; DEFINE EVENT CONSTANTS
(defvar CU-EVENT-DEFAULT 0)
(defvar CU-EVENT-BLOCKING-SYNC 1)
(defvar CU-EVENT-DISABLE-TIMING 2)


#|
;; DEFINE CUDA DATA TYPE MAPPING
typedef int CUdevice;                                     /**< CUDA device */
typedef struct CUctx_st *CUcontext;                       /**< CUDA context */
typedef struct CUmod_st *CUmodule;                        /**< CUDA module */
typedef struct CUfunc_st *CUfunction;                     /**< CUDA function */
typedef struct CUarray_st *CUarray;                       /**< CUDA array */
typedef struct CUtexref_st *CUtexref;                     /**< CUDA texture reference */
typedef struct CUsurfref_st *CUsurfref;                   /**< CUDA surface reference */
typedef struct CUevent_st *CUevent;                       /**< CUDA event */
typedef struct CUstream_st *CUstream;                     /**< CUDA stream */
typedef struct CUgraphicsResource_st *CUgraphicsResource; /**< CUDA graphics interop resource */

typedef struct CUuuid_st {                                /**< CUDA definition of UUID */
    char bytes[16];
} CUuuid;
|#


;; DEFINE DATA TYPES
(fli:define-c-typedef bool (:boolean :int))
(fli:define-c-typedef long :long)

;; DEFINE PARAMETRIZED DATA TYPES
(fli:define-foreign-type
    unsigned-byte (&optional (bitsize '*))
  (case bitsize
    (8 '(:unsigned :byte))
    (16 '(:unsigned :short))
    (32 '(:unsigned :int))
    (otherwise (error "Illegal foreign type."))))

;; DEFINE CUDA DATA TYPES
(fli:define-c-typedef cu-device :int)
(fli:define-c-typedef cu-context :int)
(fli:define-c-typedef cu-module :int)

;; DEFINE DATA STRUCTURES
(fli:define-c-struct tagCGGLglslversion  (x long)  (y long))
(fli:define-c-typedef CGGLglslversion (:struct tagCGGLglslversion))
(fli:define-c-struct tagpoint  (x long)  (y long))
(fli:define-c-typedef point (:struct tagpoint))
(fli:define-c-typedef lppoint (:pointer point))


;; REGISTER MODULES
(fli:register-module "nvcuda" :connection-style :immediate)
(fli:connected-module-pathname "nvcuda")


;; DEFINE FOREIGN FUNCTIONS
(fli:define-foreign-function (cu-init "cuInit")     
    ((flags :int))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

#|
(CU-INIT 0)
|#

(fli:define-foreign-function (cu-device-get-count "cuDeviceGetCount")
    ((ptr (:pointer :int)))
  :result-type long
  :calling-convention :cdecl)

#|
(fli:with-dynamic-foreign-objects ((object long))
  (cu-device-get-count object)
  (fli:dereference (print object)))
|#


(fli:define-foreign-function (cu-device-total-mem "cuDeviceTotalMem")
    ((ptr (:pointer :int))
     (device :int))
  :result-type long
  :calling-convention :cdecl)

#|
(fli:with-dynamic-foreign-objects ((object long))
  (cu-device-total-mem object 0)
  (fli:dereference (print object)))
|#

(fli:define-foreign-function (cu-device-get-name "cuDeviceGetName" :source)
    ((string (:reference (:ef-mb-string :limit 256)))
     (max-name :int)
     (device-index long))
  :result-type :long   
  :calling-convention :cdecl
  :module "nvcuda")

#|
(cu-device-get-name "" 256 0)
|#

(fli:define-foreign-function (cu-device-get-attribute "cuDeviceGetAttribute")     
    ((value (:pointer :int))
     (attribute :int)
     (device-index long))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

#|
(fli:with-dynamic-foreign-objects ((object :int))
  (cu-device-get-attribute object CU-DEVICE-ATTRIBUTE-MAX-THREADS-PER-BLOCK 0)
  (print (fli:dereference object)))
|#

(fli:define-foreign-function (cu-ctx-create "cuCtxCreate")     
    ((context (:pointer cu-context))
     (flags :int)
     (device cu-device))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

#|
(fli:with-dynamic-foreign-objects ((object :int))
  (cu-ctx-create object CU-CTX-SCHED-AUTO 0)
  (print (fli:dereference object)))
|#

(fli:define-foreign-function (cu-ctx-push-current "cuCtxPushCurrent")     
    ((context cu-context))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

#|
(cu-ctx-push-current 0)
|#

(fli:define-foreign-function (cu-ctx-pop-current "cuCtxPopCurrent")     
    ((context (:pointer cu-context)))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

#|
(fli:with-dynamic-foreign-objects ((object cu-context))
  (cu-ctx-pop-current object)
  (print (fli:dereference object)))
|#

(fli:define-foreign-function (cu-module-load "cuModuleLoad")     
    ((module (:pointer cu-module))
     (file (:reference (:ef-mb-string :limit 256))))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

#|
(fli:with-dynamic-foreign-objects ((object cu-module))
  (cu-module-load object "d:\\example\\vectorAdd.cu")
  (print (fli:dereference object)))
|#

(fli:define-foreign-function (cu-module-get-function "cuModuleGetFunction")     
    ((function :int)
     (module :int)
     (name (:reference (:ef-mb-string :limit 256))))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

(fli:define-foreign-function (cu-module-unload "cuModuleUnload")     
    ((value (:pointer :int))
     (attribute :int)
     (device-index long))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

(fli:define-foreign-function (cu-malloc "cuMalloc")     
    ((size :type long))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

(fli:define-foreign-function (cu-free "cuFree")     
    ((size :type long))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

(fli:define-foreign-function (cu-get-error-string "cuGetErrorString")     
    ((value :int))
  :result-type :ef-mb-string
  :module "nvcuda"
  :calling-convention :cdecl)

;(cu-get-error-string 1)















