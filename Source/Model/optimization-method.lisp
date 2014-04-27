
(defclass optimization-method (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)
   (value-function :initarg :value-function :initform nil :accessor value-function)))


(defmethod execute-optimization-on ((method optimization-method) (object t))
  (apply (value-function method) (list method object)))

(defmethod execute-optimization-on ((method optimization-method) (object list))
  (dolist (i object)
    (execute-optimization-on method i)))

(defmethod execute-optimization-on ((method optimization-method) (object population))
  (dotimes (i (count-individuals object))
    (execute-optimization-on 
     method 
     (aref (individuals-array object) i))))
