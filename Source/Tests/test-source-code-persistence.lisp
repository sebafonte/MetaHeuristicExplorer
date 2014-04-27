(defclass test-source-code-persistence (test-base-model) 
  ())


(defmethod verify-source-code-persistence ((test test-source-code-persistence) (object t) &optional (function nil))
  (declare (ignore function))
  (check
    (equals (eval (new-source-description object)) object)))

;; #TODO: Add or check which is element-equals
(defmethod test-element-equals ((a sequence) (b sequence))
  (and (= (length a) (length b))
       (let ((result t))
         (equals (pprint a) (pprint b))
         result)))

(defclass source-code-persistence-test-object (base-model)
  ((name :initarg :name :initform "Unnamed" :accessor name)
   (connections :initarg :connections :initform nil :accessor connections)))


(defmethod equals ((a source-code-persistence-test-object) (b source-code-persistence-test-object))
  (and (equal (name a) (name b))
       (test-element-equals (connections a) (connections b))))

(defmethod test-case-a ((o test-source-code-persistence))
  "Test persistence with a relationship a -> b."
  (let* ((b (make-instance 'source-code-persistence-test-object :name "B"))
         (a (make-instance 'source-code-persistence-test-object :name "A" :connections (list b))))
    (verify-source-code-persistence o a)))

(defmethod test-case-b ((o test-source-code-persistence))
  "Test persistence with a relationship a -> b, b -> a."
  (let ((a (make-instance 'source-code-persistence-test-object :name "A"))
        (b (make-instance 'source-code-persistence-test-object :name "B")))
    (setf (connections a) (list b)
          (connections b) (list a))
    (verify-source-code-persistence o a)))

(defmethod test-case-c ((o test-source-code-persistence))
  "Test persistence with a relationship a -> b, b -> c, c -> a."
  (let ((a (make-instance 'source-code-persistence-test-object :name "A"))
        (b (make-instance 'source-code-persistence-test-object :name "B"))
        (c (make-instance 'source-code-persistence-test-object :name "C")))
    (setf (connections a) (list b)
          (connections b) (list c)
          (connections c) (list a))
  (verify-source-code-persistence o a)))

(defmethod test-case-d ((o test-source-code-persistence))
  "Test persistence with a relationship a -> b, b -> c, b -> a, c -> a."
  (let ((a (make-instance 'source-code-persistence-test-object :name "A"))
        (b (make-instance 'source-code-persistence-test-object :name "B"))
        (c (make-instance 'source-code-persistence-test-object :name "C")))
    (setf (connections a) (list b)
          (connections b) (list c)
          (connections b) (list a)
          (connections c) (list a))
    (verify-source-code-persistence o a)))

(defmethod test-case-e ((o test-source-code-persistence))
  "Test persistence with a relationship a -> b, b -> a, b -> c, c -> b."
  (let ((a (make-instance 'source-code-persistence-test-object :name "A"))
        (b (make-instance 'source-code-persistence-test-object :name "B"))
        (c (make-instance 'source-code-persistence-test-object :name "C")))
    (setf (connections a) (list b)
          (connections b) (list a)
          (connections b) (list c)
          (connections c) (list b))
    (verify-source-code-persistence o a)))

(defmethod test-case-f ((o test-source-code-persistence))
  "Test persistence with a relationship a -> b, a -> c, b -> a, c -> a."
  (let ((a (make-instance 'source-code-persistence-test-object :name "A"))
        (b (make-instance 'source-code-persistence-test-object :name "B"))
        (c (make-instance 'source-code-persistence-test-object :name "C")))
    (setf (connections a) (list b)
          (connections a) (list c)
          (connections b) (list a)
          (connections c) (list a))
    (verify-source-code-persistence o a)))

(defmethod test-case-g ((o test-source-code-persistence))
  "Test persistence with a relationship a -> b, a -> c, b -> a, b -> c, c -> a, c -> b."
  (let ((a (make-instance 'source-code-persistence-test-object :name "A"))
        (b (make-instance 'source-code-persistence-test-object :name "B"))
        (c (make-instance 'source-code-persistence-test-object :name "C")))
    (setf (connections a) (list b)
          (connections a) (list c)
          (connections b) (list a)
          (connections b) (list c)
          (connections c) (list a)
          (connections c) (list b))
    (verify-source-code-persistence o a)))

(defmethod test-case-editable-probability-object-list-wrapper-subject ((o test-source-code-persistence))
  "Test subject persistence of a 'editable-probability-object-list-wrapper instance."
  (eval
   (new-source-description 
    (make-instance 'editable-probability-object-list-wrapper :subject nil))))

