
(defclass test-container (base-model)
  ((name :initarg :name :initform "" :accessor name)
   (test-method :initarg :test-method :accessor test-method)
   (test-class :initarg :test-class :accessor test-class)
   (test-result :initarg :test-result :accessor test-result)))


(defmethod equals ((a test-container) (b test-container))
  (and (equal (test-method a) (test-method b))
       (equal (test-class a) (test-class b))))

(defmethod run-case ((o test-container))
  (funcall (name-symbol (test-method o)) 
           (make-instance (class-name (test-class o)))))

(defmethod run-test ((o test-container))
  "Executes tests of <o>."
  (run-case-with (test-result o) o))

(defmethod debug-test ((o test-container))
  "Executes tests of <o>."
  (debug-case-with (test-result o) o))

(defmethod get-info-test ((o test-container))
  "Answer a list with <o> basic information."
  (append 
   (list (name o))
   (get-info-test (test-result o))))
