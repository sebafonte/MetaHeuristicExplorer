
(defclass test-suite ()
  ((name :initarg :name :initform "Test suite" :accessor name)
   (category :initarg :category :initform nil :accessor category)
   (test-case-classes :initarg :test-case-classes :accessor test-case-classes)))


(defmethod print-object ((o test-suite) seq)
  (format seq "~A" (name o)))

(defmethod run-suite ((o test-suite))
  nil)

(defmethod resources ((o test-suite))
  nil)

(defmethod resources-availible ((o test-suite))
  t)

(defmethod test-containers ((o test-suite))
  (unique-equals
   (let ((union))
     (dolist (test-case-class (test-case-classes o))
       (appendf union (test-containers test-case-class)))
     union)))

(defmethod add-test-case ((o test-suite) (test t))
  (appendf (test-case-classes o) test))

(defun behaviour-default-test-suites ()
  (list (system-get 'test-suite-system)))

(defun performance-default-test-suites ()
  (list (system-get 'test-suite-system-functions-performance)
        (system-get 'test-suite-default-search-tasks-performance)
        (system-get 'test-suite-environment-performance)
        (system-get 'test-suite-host-performance)))
