
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
  ((current-item :initarg :current-item :accessor current-item)
   (log-data :initarg :log-data :initform (make-instance 'log-data-container) :accessor log-data)
   (log-inspectors :initarg :log-inspectors :initform nil :accessor log-inspectors)))


(defmethod initialize-properties :after ((o base-benchmark))
  "Initialize <object> properties.
   #NOTE: these properties should return the function to obatin the value for the argument (a task)."
  (add-properties-from-values
   o
   (:name 'item :label "Item" :accessor-type 'valuable-accessor-type :visible nil
    :data-type 'object :getter (lambda (object) (current-item o)))))

(defmethod abstractp ((o (eql 'base-benchmark)))
  "Answer whether <o> is abstract."
  t)

(defmethod initialize-log-inspectors ((o base-benchmark) subject)
  nil)

(defmethod inspect-object ((o base-benchmark))
  (dolist (i (log-inspectors o))
    (apply-log-inspector-on i)))

(defmethod prepare-benchmark ((o base-benchmark) subject)
  "Connect <task> events and initialize <task> data dictionary."
  (initialize-log-inspectors o subject)
  (inspect-object o)
  (setf (benchmarker subject) o))

#| 
;;; Other properties to put

;; Presence of building blocks
 (:name 'building-block-a :label "Time" :accessor-type 'property-accessor-type
  :data-type 'number :default-value 0)
 (:name 'building-block-b :label "Time" :accessor-type 'property-accessor-type
  :data-type 'number :default-value 0)
|#
