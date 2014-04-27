(defclass constant-factory (object-with-properties)
  ((name :initarg :name :accessor name)
   (distribution :initarg :distribution :accessor distribution)))


(defmethod abstractp ((o (eql 'constant-factory)))
  "Answer whether <o> is abstract."
  t)

(defmethod print-object ((r constant-factory) seq)
  (format seq "~A" (name r)))


