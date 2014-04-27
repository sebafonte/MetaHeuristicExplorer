(fli:register-module " cg.dll ") 
(fli:register-module " cgfx.dll ") 
(fli:register-module #p"D:/DATA/cg.DLL" :connection-style :immediate)


(fli:define-foreign-function (multiply "multiply")     
    ((x :int)      
     (y :int)) 
  :result-type int   
  :module :cg-dll   
  :calling-convention :cdecl)



;; LISTA CON FUNCIONES DE REFERENCIA PARA PASAR: http://http.developer.nvidia.com/Cg/cgGLSetParameter4fv.html
;; 
;; CGprogram program = cgCreateProgram(context,             // from cgCreateContext
;;                                     CG_SOURCE,           // type: source or object
;;                                     programString,       // program text/data
;;                                     profile,             // profile
;;                                     "main",              // entry function name
;;                                     args);               // compiler options
;; CGparameter myParameter = cgGetNamedParameter(program, "myParameter");
;; cgGLSetParameter4fv(myParameter, value);
;; cgGLEnableProfile(CG_PROFILE_ARBVP1);
;; cgGLDisableProfile(CG_PROFILE_ARBVP1);
;; cgGLBindProgram(program);
;; cgDestroyProgram(program);
;; cgDestroyContext(context);
;; CGerror error = cgGetError();
;; const char* errorString = cgGetErrorString(error);

;; CGGLglslversion cgGLDetectGLSLVersion( void );

(fli:define-c-typedef bool (:boolean :int))
(fli:define-c-typedef long :long)
(fli:define-c-struct tagpoint  (x long)  (y long))
(fli:define-c-typedef point (:struct tagpoint))
(fli:define-c-typedef lppoint (:pointer point))
(fli:define-c-struct tagCGGLglslversion  (x long)  (y long))
(fli:define-c-typedef CGGLglslversion (:struct tagCGGLglslversion))

(fli:define-foreign-function (cg-gl-detect-glsl-version "cgGLDetectGLSLVersion")     
    () 
  :result-type CGGLglslversion
  :module :cg-dll
  :calling-convention :cdecl)

;; ----------

(setq point5 (fli:allocate-foreign object :type :int))
(setf (fli:dereference point5) 12)
(fli:dereference point5)

(fli:define-foreign-function (count-upper "count_upper" :source)
    ((string (:reference-pass :ef-mbstring)))
  :result-type :int
  :language :c
  :calling-convention :cdecl)

;; ----------


(fli:define-foreign-function (get-cursor-position "GetCursorPos") 
    ((lp-point lppoint))  
  :result-type bool)

(setq location (fli:allocate-foreign-object :type 'point))

(get-cursor-position location)

(fli:foreign-slot-value location 'x)
(fli:foreign-slot-value location 'y)

(fli:define-foreign-function (set-cursor-position "SetCursorPos")   
    ((x :long)
     (y :long))  :result-type :boolean)

(defun test-cursor ()  
  (dotimes (x 10)    
    (dotimes (d 300)    
      (let ((r (/ (+ d (* 300 x)) 10.0)))   
        (set-cursor-position       
         (+ 300 (floor (* r (cos (/ (* d pi) 150.0)))))     
         (+ 300 (floor (* r (sin (/ (* d pi) 150.0))))))))))

(test-cursor)

(fli:free-foreign-object location)

(defun current-cursor-position ()  
  (fli:with-dynamic-foreign-objects ()  
    (let ((lppoint (fli:allocate-dynamic-foreign-object               
                    :pointer-type 'lppoint)))  
      (if (get-cursor-position lppoint)       
          (values t (fli:foreign-slot-value lppoint 'x)   
                  (fli:foreign-slot-value lppoint 'y))     
        (values nil 0 0)))))
