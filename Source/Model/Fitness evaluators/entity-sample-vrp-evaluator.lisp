
(defclass entity-sample-vrp-evaluator (entity-evaluator)
  ((load-vrp-description-from-file :accessor load-vrp-description-from-file)
   (cities-description :initarg :cities-description :accessor cities-description)
   (costs-matrix :initarg :costs-matrix :accessor costs-matrix)
   (vehicle-cost :initarg :vehicle-cost :accessor vehicle-cost)
   (return-to-depot :initarg :return-to-depot :accessor return-to-depot)
   (time-factor :initarg :time-factor :accessor time-factor)
   (max-vehicles :initarg :max-vehicles :accessor max-vehicles)
   (max-vehicles-penalty-cost :initarg :max-vehicles-penalty-cost :accessor max-vehicles-penalty-cost)
   (fitness-function :initarg :fitness-function :accessor fitness-function)
   (precision :initarg :precision :accessor precision)))


(defmethod initialize-properties :after ((object entity-sample-vrp-evaluator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'load-vrp-description-from-file :label "Problem file" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'file-prompter-editor :setter '(setf load-vrp-description-from-file))
   (:name 'cities-description :label "Cities description" :accessor-type 'accessor-accessor-type 
    :editor 'lisp-editor :data-type 'list-structure :default-value '((0 0) (0 3) (3 0)))
   (:name 'solution-fitness :label "Solution fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 1 :max-value 1000000 :default-value 0 :editor 'number-editor)
   (:name 'vehicle-cost :label "Vehicle cost" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 1000000 :default-value 0 :editor 'number-editor)
   (:name 'return-to-depot :label "Return to depot" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value t :editor 'number-editor)
   (:name 'min-fitness :label "Min fitness" :accessor-type 'accessor-accessor-type
    :data-type 'number :min-value 0 :max-value 1000000 :default-value 0 :editor 'number-editor)
   (:name 'max-fitness :label "Max fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 1000000 :default-value 200 :editor 'number-editor)
   (:name 'max-vehicles :label "Max vehicles" :accessor-type 'accessor-accessor-type
    :data-type 'integer :min-value 1 :max-value 1000 :default-value 3 :editor 'number-editor)
   (:name 'max-vehicles-penalty-cost :label "Max vehicles penalty" :accessor-type 'accessor-accessor-type
    :data-type 'number :min-value 0 :max-value 1000000 :default-value 1000 :editor 'number-editor)
   (:name 'time-factor :label "Time factor" :accessor-type 'accessor-accessor-type
    :data-type 'number :min-value 0 :max-value 1 :default-value 0.95 :editor 'number-editor)
   (:name 'fitness-function :label "Fitness function" :default-value 'execute-simulation-mixed 
    :possible-values (possible-fitness-functions object) :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'list-editor)
   (:name 'precision :label "Accuracy" :default-value 'float :possible-values '(number double-float float single-float)
    :accessor-type 'accessor-accessor-type :data-type 'symbol :editor 'list-editor)))

(defmethod possible-fitness-functions ((o entity-sample-vrp-evaluator))
  "Answer <o> possible fitness-functions."
  '(execute-simulation-distance 
    execute-simulation-time 
    execute-simulation-mixed))

(defmethod (setf load-vrp-description-from-file) (file (o entity-sample-vrp-evaluator))
  "Sets the initial file string in initial-matrix-file slot of <o>."
  (when file
    (setf (slot-value o 'cities-description) (load-from-file file :tag :cities-description))))

(defmethod initialize-fitness-data ((o entity-sample-vrp-evaluator))
  "Initialize <o> fitness data."
  (let ((cities-count (cities-count o)))
    (setf (costs-matrix o) (make-array (list cities-count cities-count) :element-type (precision o)))
    (if (euclidean-distance-mode o)
        (initialize-fitness-data-euclidean o))))

(defmethod initialize-fitness-data-euclidean ((o entity-sample-vrp-evaluator))
  (let ((cities-count (cities-count o)))
    (dotimes (i cities-count)
      (dotimes (j cities-count)
        (setf (aref (costs-matrix o) i j) 
              (euclidean-distance-between o i j))))))

(defmethod euclidean-distance-between ((o entity-sample-vrp-evaluator) i j)
  "Answer the cost for travelling from city <i> to city <j> using <o>."
  (let* ((a-coords (nth i (cities-description o)))
         (b-coords (nth j (cities-description o)))
         (a-x (first a-coords))
         (a-y (second a-coords))
         (b-x (first b-coords))
         (b-y (second b-coords)))
    (sqrt (+ (sqr (- b-x a-x))
             (sqr (- b-y a-y))))))

(defmethod euclidean-distance-mode ((o entity-sample-vrp-evaluator))
  (atom (first (demand-description o))))

(defmethod cities-count ((o entity-sample-vrp-evaluator))
  "Answer cities count of <o>."
  (length (cities-description o)))

(defmethod evaluate ((evaluator entity-sample-vrp-evaluator) (o entity-sample-vrp))
  "Use <evaluator> to calculate and answer <object> fitness."
  (execute-simulation evaluator o))

(defmethod execute-simulation ((evaluator entity-sample-vrp-evaluator) (o entity-sample-vrp))
  "Execute <o> simulation to obtain it's final state."
  (funcall (fitness-function evaluator) evaluator o))

(defun execute-simulation-distance (evaluator o)
  "Execute <o> simulation to obtain it's final state.
   NOTE: Priorize total distance and penalize max vehicles."
  (let ((total-cost 0)
        (costs-matrix (costs-matrix evaluator))
        (return-to-depot (return-to-depot evaluator))
        (tour (program o)))
    (dolist (vehicle-plan tour)
      (let ((actual-city 0)
            (vehicle-cost (vehicle-cost evaluator)))
        ;; Tour cost
        (dolist (next-city vehicle-plan)
          (incf vehicle-cost (aref costs-matrix actual-city next-city))
          (setf actual-city next-city))
        ;; Return to depot cost
        (if return-to-depot 
            (incf vehicle-cost (aref costs-matrix actual-city 0)))
        (incf total-cost vehicle-cost)))
    ;; Penalty cost
    (if (> (length tour) (max-vehicles evaluator)) 
        (incf total-cost (max-vehicles-penalty-cost evaluator)))
    (setf (fitness o) total-cost)))

(defun execute-simulation-time (evaluator o)
  "Execute <o> simulation to obtain it's final state.
   It's assumed that vehicle-cost is measured in seconds."
  (let ((costs-matrix (costs-matrix evaluator))
        (return-to-depot (return-to-depot evaluator))
        (tour (program o))
        (total-cost 0))
    (dolist (vehicle-plan tour)
      (let ((actual-city 0)
            (vehicle-time (vehicle-cost evaluator)))
        ;; Tour cost
        (dolist (next-city vehicle-plan)
          (incf vehicle-time (aref costs-matrix actual-city next-city))
          (setf actual-city next-city))
        ;; Return to depot cost
        (if return-to-depot 
            (incf vehicle-time (aref costs-matrix actual-city 0)))
        (setf total-cost (max vehicle-time total-cost))))
    ;; Calculate cost (in time units)
    (if (> (length tour) (max-vehicles evaluator)) 
        (incf total-cost (max-vehicles-penalty-cost evaluator)))
    (setf (fitness o) total-cost)))

(defun execute-simulation-mixed (evaluator o)
  "Execute <o> simulation to obtain it's final state."
  (let ((costs-matrix (costs-matrix evaluator))
        (return-to-depot (return-to-depot evaluator))
        (tour (program o))
        (max-time 0)
        (total-time 0)
        (time-factor (time-factor evaluator)))
    (declare (max-time number) (total-time number))
    (dolist (vehicle-plan tour)
      (let ((actual-city 0)
            (vehicle-time (vehicle-cost evaluator)))
        (declare (vehicle-time number))
        ;; Tour time
        (dolist (next-city vehicle-plan)
          (incf vehicle-time (aref costs-matrix actual-city next-city))
          (setf actual-city next-city))
        ;; Return to depot cost
        (if return-to-depot 
            (incf vehicle-time (aref costs-matrix actual-city 0)))
        (setf max-time (max vehicle-time max-time))
        (incf total-time vehicle-time)))
    ;; Calculate cost (in time units)
    (setf total-cost (+ (* time-factor max-time)
                        (* (- 1 time-factor) total-time)))
    (if (> (length tour) (max-vehicles evaluator))
        (incf total-cost (max-vehicles-penalty-cost evaluator)))
    ;; Assign fitness to o
    (setf (fitness o) total-cost)))

(defun execute-simulation-vrp (evaluator o)
  "Execute <o> simulation to obtain it's final state.
   It's assumed that vehicle-cost is measured in seconds."
  (let ((costs-matrix (costs-matrix evaluator))
        (return-to-depot (return-to-depot evaluator))
        (tour (program o))
        (total-cost 0))
    (declare (total-cost number))
    (dolist (vehicle-plan tour)
      (let ((actual-city 0)
            (vehicle-time (vehicle-cost evaluator)))
        ;; Tour cost
        (dolist (next-city vehicle-plan)
          (incf vehicle-time (aref costs-matrix actual-city next-city))
          (setf actual-city next-city))
        ;; Return to depot cost
        (if return-to-depot 
            (incf vehicle-time (aref costs-matrix actual-city 0)))
        (setf total-cost (max vehicle-time total-cost))))
    ;; Calculate cost (in time units)
    (if (> (length tour) (max-vehicles evaluator)) 
        (incf total-cost (max-vehicles-penalty-cost evaluator)))
    (setf (fitness o) total-cost)))