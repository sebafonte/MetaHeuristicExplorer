

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



(fli:define-c-typedef bool (:boolean :int))
(fli:define-c-typedef long :long)
(fli:define-c-struct tagpoint  (x long)  (y long))
(fli:define-c-typedef point (:struct tagpoint))
(fli:define-c-typedef lppoint (:pointer point))

(fli:define-foreign-type
    unsigned-byte (&optional (bitsize '*))
  (case bitsize
    (8 '(:unsigned :byte))
    (16 '(:unsigned :short))
    (32 '(:unsigned :int))
    (otherwise (error "Illegal foreign type."))))

(fli:define-c-struct tagCGGLglslversion  (x long)  (y long))
(fli:define-c-typedef CGGLglslversion (:struct tagCGGLglslversion))


;;
;; D:/Windows/syswow64/
;;
;;
;;(fli:register-module "d:/cg/gl.dll" :connection-style :immediate)
;;(fli:connected-module-pathname "cg")
;;
;;
;;(fli:define-foreign-function (cg-gl-detect-glsl-version "cgGLDetectGLSLVersion")     
;;    ((string (:reference-pass :ef-mbstring))) 
;;  :result-type CGGLglslversion
;;  :module :cg-dll
;;  :calling-convention :cdecl)
;;

(fli:register-module "nvcuda" :connection-style :immediate)
(fli:connected-module-pathname "nvcuda")


(fli:define-foreign-function (cu-init "cuInit")     
    ((flags :int))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

;(CU-INIT 0)

(fli:define-foreign-function (init-alloc "cuDeviceGetCount")
    ((ptr (:pointer :int)))
  :result-type long
  :calling-convention :cdecl)

#|
(fli:with-dynamic-foreign-objects ((object long))
  (cu-device-get-count object)
  (fli:dereference (print object)))
|#

(fli:define-foreign-function (cu-device-get-name "cuDeviceGetName")     
    ((name (:pointer :ef-mb-string))
     (max-name :int)
     (device-index long))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)

(fli:define-foreign-function (cu-device-get-name "cuDeviceGetName" :source)
    ((string (:reference (:ef-mb-string :limit 256)))
     (max-name :int)
     (device-index long))
  :result-type :long   
  :calling-convention :cdecl
  :module "nvcuda")

;(cu-device-get-name "" 256 0)


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


(fli:define-foreign-function (cu-module-load "cuModuleLoad")     
    ((value (:pointer :int))
     (attribute :int)
     (device-index long))
  :result-type long
  :module "nvcuda"
  :calling-convention :cdecl)




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






typedef enum CUdevice_attribute_enum {
    CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 1,             
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X = 2,                   
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y = 3,                   
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z = 4,                   
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X = 5,                    
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y = 6,                    
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z = 7,                    
    CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK = 8,       
    CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK = 8,           
    CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY = 9,    
    CU_DEVICE_ATTRIBUTE_WARP_SIZE = 10,               
    CU_DEVICE_ATTRIBUTE_MAX_PITCH = 11,               
    CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK = 12, 
    CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK = 12,     
    CU_DEVICE_ATTRIBUTE_CLOCK_RATE = 13,              
    CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT = 14,       
    CU_DEVICE_ATTRIBUTE_GPU_OVERLAP = 15,             
    CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT = 16,    
    CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT = 17,     
    CU_DEVICE_ATTRIBUTE_INTEGRATED = 18,              
    CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY = 19,     
    CU_DEVICE_ATTRIBUTE_COMPUTE_MODE = 20,            
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_WIDTH = 21, 
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_WIDTH = 22, 
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_HEIGHT = 23,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_WIDTH = 24, 
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_HEIGHT = 25,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_DEPTH = 26, 
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_WIDTH = 27,     /**< Maximum texture array width */
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_HEIGHT = 28,    /**< Maximum texture array height */
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_NUMSLICES = 29, /**< Maximum slices in a texture array */
    CU_DEVICE_ATTRIBUTE_SURFACE_ALIGNMENT = 30,                 /**< Alignment requirement for surfaces */
    CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS = 31,                /**< Device can possibly execute multiple kernels concurrently */
    CU_DEVICE_ATTRIBUTE_ECC_ENABLED = 32,                       /**< Device has ECC support enabled */
    CU_DEVICE_ATTRIBUTE_PCI_BUS_ID = 33,                        /**< PCI bus ID of the device */
    CU_DEVICE_ATTRIBUTE_PCI_DEVICE_ID = 34,                     /**< PCI device ID of the device */
    CU_DEVICE_ATTRIBUTE_TCC_DRIVER = 35                         /**< Device is using TCC driver model */

#if __CUDA_API_VERSION >= 4000
    , 
    CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE = 36,                 /**< Peak memory clock frequency in kilohertz */
    CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH = 37,           /**< Global memory bus width in bits */
    CU_DEVICE_ATTRIBUTE_L2_CACHE_SIZE = 38,                     /**< Size of L2 cache in bytes */
    CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_MULTIPROCESSOR = 39,    /**< Maximum resident threads per multiprocessor */
    CU_DEVICE_ATTRIBUTE_ASYNC_ENGINE_COUNT = 40,                /**< Number of asynchronous engines */
    CU_DEVICE_ATTRIBUTE_UNIFIED_ADDRESSING = 41,                /**< Device uses shares a unified address space with the host */    
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_LAYERED_WIDTH = 42,   /**< Maximum 1D layered texture width */
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_LAYERED_LAYERS = 43   /**< Maximum layers in a 1D layered texture */
#endif
} CUdevice_attribute;






(fli:define-foreign-function (cu-get-error-string "cuGetErrorString")     
    ((value :int))
  :result-type :ef-mb-string
  :module "nvcuda"
  :calling-convention :cdecl)

;(cu-get-error-string 1)















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

