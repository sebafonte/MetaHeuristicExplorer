
(defclass test-result (base-model)
  ((status :initarg :status :initform nil :accessor status)
   (running-time :initarg :running-time :initform 0 :accessor running-time)))


(defmethod abstractp ((o (eql 'test-result)))
  "Answer whether <o> is abstract."
  t)

(defmethod get-info-test ((o test-result))
  (list (status o)
        (running-time o)))

(defmethod debug-case-with ((r test-result) (o test-container))
  "Executes tests of <o>."
  (setf (status r) 'DEBUGGING)
  (run-case o)
  (setf (status r) 'OK))


(defclass behaviour-test-result (test-result)
  ())

(defmethod run-case-with ((result behaviour-test-result) (object t))
  (setf (status result) 'RUNNING)
  (let ((status 'OK)
        (initial-time (get-internal-real-time)))
    (handler-case (run-case object)
      (assertion-failed () (setf status 'FAILED))
      (error (function) (setf status 'ERROR)))
    (setf (running-time result) (- (get-internal-real-time) initial-time)
          (status result) status)))


(defclass performance-test-result (test-result base-model)
  ((benchmark-result :initarg :benchmark-result :initform nil :accessor benchmark-result)))


(defmethod get-info-test ((o performance-test-result))
  (list (status o)
        (running-time o)
        (benchmark-result o)))

(defmethod run-case-with ((result performance-test-result) (object t))
  (setf (status result) 'RUNNING)
  (let ((test-result)
        (status 'OK)
        (initial-time (get-internal-real-time)))
    (handler-case (setf test-result (run-case object))
      (assertion-failed () 
        (setf status 'FAILED))
      (error (function) 
        (setf status 'ERROR)))
    (setf (status result) status
          (running-time result) (- (get-internal-real-time) initial-time)
          (benchmark-result result) test-result)))