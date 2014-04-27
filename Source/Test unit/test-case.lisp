
(defparameter *test-output* *standard-output*)


(defclass test-case (standard-object)
  ((name :initarg :name :initform "Test case" :accessor name)
   (category :initarg :category :initform nil :accessor category)
   (test-containers :initarg :test-containers :accessor test-containers)))


(defmethod unit-test-abstract ((o (eql 'test-case)))
  "Answer whether <o> is abstract."
  t)

(defmethod unit-test-abstract ((o t))
  "Answer whether <o> is abstract."
  nil)

;; #TODO: Remove make-instance, move this behaviour to class side
(defmethod classes-for-tests ((o test-case))
  (let ((result))
    (if (not (unit-test-abstract (class-of o))) 
        (push o result))
    (dolist (i (clos:class-direct-subclasses (class-of o)))
      (setf result (append (classes-for-tests (make-instance i)) result)))
    result))

(defmethod methods-for-tests ((o test-case))
  "Answer a list with all <o> test-methods and it's subclasses."
  (let ((result))
    ;; Process direct methods
    (if (not (unit-test-abstract o))
          (setf result (direct-test-methods o)))
    ;; Process subclasses
    (dolist (i (clos:class-direct-subclasses (class-of o)))
      (let ((instance (make-instance (class-name i))))
        (if (not (unit-test-abstract instance))
            (setf result (append result (methods-for-tests instance))))))
    result))
                              
(defmethod test-containers ((o test-case))
  (mapcar 
   (lambda (i) (make-instance 'test-container 
                              :name (name (car i))
                              :test-class (cadr i)
                              :test-method (car i)
                              :test-result (result-for-case o)))
   (select (methods-for-tests o)
           (lambda (object) 
             (not (null (car object)))))))

(defmethod direct-test-methods ((o test-case))
  "Anwser a list with a list of conses in the form '(test-method test-class), a test 
   description for class o."
  (mapcar 
   (lambda (object) (list object (class-of o)))
   (select (clos:class-direct-methods (class-of o))
           (lambda (element) 
             (let ((prefix (test-case-test-prefix o)))
               (and (>= (length (name element)) (length prefix))
                    (equal (subseq (name element) 0 (length prefix)) prefix)))))))

(defmethod test-case-test-prefix ((o test-case))
  "TEST-")

;; RESULT DELEGATION SHOULD BE DONE HERE
(defmethod run-case ((o test-case))
  (set-up o)
  (run-test-containers o)
  (tear-down o))

(defmethod run-test-containers ((o test-case))
  (dolist (i (test-containers o))
    (run-case i)))

(defmethod run-test ((o test-case))
  (run-case-with result o))

(defmethod set-up ((o test-case))
  nil)
  
(defmethod tear-up ((o test-case))
  nil)

(defmethod resources ((o test-case))
  nil)

(defmethod test-assert ((o test-case))
  nil)

(defmethod test-deny ((o test-case))
  nil)

(defmethod test-should ((o test-case))
  nil)

(defmethod test-shouldnt ((o test-case))
  nil)

(defmethod result-for-case ((o test-case))
  (make-instance 'behaviour-test-result))
