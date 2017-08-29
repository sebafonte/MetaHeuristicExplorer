
(defclass steepest-descent-optimization (optimization-method)
  ((max-iterations :initarg :max-iterations :initform 20 :accessor max-iterations)
   (precision :initarg :precision :initform 0.01 :accessor precision)
   (delta-gradient :initarg :delta-gradient :initform 0.01 :accessor delta-gradient)))


(defun optimize-constants-steepest-descent-1 (method object)
  (let ((copy (copy object)))
    (setf (program copy) (cons 'values (list (program (object copy))))
          copy (constant-optimization-steepest-descent copy method))
    (let ((object-copy (object copy))
          (object-object (object object)))
      (setf (program object-copy) (cadr (program object-copy))
            (program object-object) (program object-copy)
            (fitness object-object) (fitness object-copy))))
  nil)
