(defclass property-function ()
  ((lisp-function :initarg :lisp-function :initform nil :accessor lisp-function)
   (code :initarg :code :initform 'nil :accessor code)))


(defmethod lisp-function ((object property-function))
  (if (null function)
      (setf (function object) 
            (compile (code object)))))