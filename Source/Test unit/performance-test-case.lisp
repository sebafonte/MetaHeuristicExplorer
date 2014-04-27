(defclass performance-test-case (test-case base-model)
  ((execution-time :initarg :execution-time :initform nil :accessor execution-time)
   (machine-descriptor :initarg :machine-descriptor :initform nil :accessor machine-descriptor)))


(defmethod unit-test-abstract ((o performance-test-case))
  nil)

(defmethod test-case-test-prefix ((o performance-test-case))
  "PERFORMANCE-TEST-")

(defmethod result-for-case ((o performance-test-case))
  (make-instance 'performance-test-result))

(defmethod unit-test-abstract ((o (eql 'performance-test-case)))
  "Answer whether <o> is abstract."
  t)
