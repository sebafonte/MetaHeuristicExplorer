
;; Events at task level, slightly at algorithm level
(defparameter *task-measures-likelihood* nil)
;; Events for basic algorithms
(defparameter *algorithm-measures-custom* nil)
;; Events intended for deep monitoring
(defparameter *algorithm-measures-extended* nil)


;; Benchmark base
(defclass benchmark-set (object-with-properties)
  ())

(defclass benchmark-result (object-with-properties)
  ())


;; Benchmark objects
(defclass base-benchmark (object-with-properties)
  ((log-inspectors :initarg :log-inspectors :initform nil :accessor log-inspectors)))


(defmethod abstractp ((o (eql 'base-benchmark)))
  "Answer whether <o> is abstract."
  t)

(defmethod initialize-instance :after ((o base-benchmark) &rest args)
  (initialize-log-inspectors o))

(defmethod measure ((o base-benchmark) (a search-algorithm))
  (let ((result (make-instance 'algorithm-efficience)))
    (dolist (i (measure-properties o))
      (set-value-for-property))))

(defmethod active-events ((b base-benchmark) object)
  nil)

(defmethod initialize-log-inspectors ((o base-benchmark))
  nil)

(defmethod inspect-object ((o base-benchmark) target)
  (dolist (i (log-inspectors o))
    (apply-log-inspector-on i target)))

(defmethod prepare-benchmark ((o base-benchmark) task)
  "Connect <task> events and initialize <task> data dictionary."
  (initialize-log-inspectors o)
  (setf (log-data task) (log-data o)))









(defmethod save-log-data-set ((o search-task) &rest args)
  "Save log-data descripted by <args> in <o>."
  (let ((arguments))
    (do ((i args (cddr i)))
        ((null i))
      (push (list (car i) (cadr i)) arguments))
    (add-value (log-data o) (make-instance 'log-data :data arguments))))


#| 
;;; Other properties to put

;; Presence of building blocks
 (:name 'building-block-a :label "Time" :accessor-type 'property-accessor-type
  :data-type 'number :default-value 0)
 (:name 'building-block-b :label "Time" :accessor-type 'property-accessor-type
  :data-type 'number :default-value 0)
|#
