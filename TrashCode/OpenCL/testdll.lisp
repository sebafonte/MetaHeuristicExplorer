#+nil
(rename-package "TDLL" (gensym))


(defpackage "TDLL"
  (:export "FOOB"))

(in-package "TDLL")

(defparameter +c-types+
  '((STR          (:reference-pass :ef-mb-string))
    (INT          (:signed-integer-type 32))
    (UINT         (:unsigned-integer-type 32))
    (PTR          :pointer)
    (LISP-ARRAY   :lisp-array)))

(defparameter +c-functions+
  '((|_foob@12| INT 
            ((INT count) 
             ((:POINTER (:UNSIGNED :CHAR)) strings) 
             ((INT) error-returned)))))

;;; Create all bindings
(defmacro doit ()
  `(progn
     ,@(loop for l in +c-types+ collect
             `(fli:define-c-typedef ,(first l) ,(second l)))
     ,@(loop for l in +c-functions+ collect
             `(fli:define-foreign-function (,(first l) ,(format nil "~A" (first l)))
                  ,(loop for p in (third l)
                         collect `(,(second p)
                                   ,(if (atom (first p))
                                        (first p)
                                      `(:pointer ,(first (first p))))))
                :result-type ,(second l)
                :module "TDLL"
                :calling-convention #+win32 :stdcall #-win32 :cdecl))))

(defun convert-strings-to-foreign-array (strings &key (allocation :static))
  (let* ((count (length strings))
         (array (fli:allocate-foreign-object 
                 :type '(:pointer (:unsigned :char))
                 :nelems (1+ count)
                 :initial-element nil
                 ;:allocation allocation
                 )))
    (loop for index from 0
          for string in strings
          do (setf (fli:dereference array :index index)
                   (fli:convert-to-foreign-string
                    string
                    :external-format :utf-8
                    ;:allocation allocation
                    )))
    array))

(doit)

#+nil
(pprint (sort (let (r) (do-symbols (s *package* r)
                         (when (eq (symbol-package s) *package*)
                           (push s r))
                         r))
              #'string>))

(fli:register-module "TDLL"
                     :real-name #+win32 "D:\\TestString\\ddd\\Debug\\dddd.dll"
                     :connection-style :immediate)

(fli:with-dynamic-foreign-objects 
    ((size tdll::int 3) 
     (error tdll::int))
  (let ((array (convert-strings-to-foreign-array (list "aa" "bbccc" "cc") :allocation :dynamic)))
    (setf result (|_foob@12| 2 array error))
    (setf ss (list result error))))

#|


(fli:disconnect-module "TDLL")

(fli:define-foreign-function _fooc 
    ((a :int) 
     (b (:POINTER (:UNSIGNED :CHAR)))
     (c (:reference-return :int)))
 :result-type :int)
|# 
