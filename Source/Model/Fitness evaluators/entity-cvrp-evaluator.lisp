
(defclass entity-vrp-evaluator (entity-evaluator)
  ((load-vrp-description-from-file :accessor load-vrp-description-from-file)
   (cities-description :initarg :cities-description :accessor cities-description)
   (demand-description :initarg :demand-description :accessor demand-description)
   (demand-matrix :initarg :demand-matrix :accessor demand-matrix)
   (costs-matrix :initarg :costs-matrix :accessor costs-matrix)
   (max-capacity :initarg :max-capacity :accessor max-capacity)
   (infeasible-penalty :initarg :infeasible-penalty :accessor infeasible-penalty)
   (back-to-depot :initarg :back-to-depot :accessor back-to-depot)
   (precision :initarg :precision :accessor precision)))


(defmethod initialize-properties :after ((object entity-vrp-evaluator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'load-vrp-description-from-file :label "Initial file" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'file-prompter-editor :setter '(setf load-vrp-description-from-file))
   (:name 'cities-description :label "Cities description" :accessor-type 'accessor-accessor-type 
    :editor 'lisp-editor :data-type 'list-structure 
    :default-value '((0 0) (27.9 10.5) (36.4 76.4) (61.7 0.9) (7.3 42.7) (51.4 20.6) (74.0 72.6)
                     (10.4 5.3) (93.0 13.6) (44.0 36.5) (60.9 1.8) (7.3 87.5) (96.5 13.6) (78.9 11.0) 
                     (47.5 23.0) (35.5 37.4) (75.3 54.4) (87.1 33.6) (6.3 9.1) (15.1 86.4)))
   (:name 'demand-description :label "Demand description" :accessor-type 'accessor-accessor-type 
    :editor 'lisp-editor :data-type 'list-structure 
    :default-value '(0 240.1 709.8 244.4 33.0 722.6 26.3 801.9 791.7 666.0 397.6 421.2 848.2 389.9 
                       84.2 597.3 667.8 315.6 743.1 51.5))
   (:name 'back-to-depot :label "Back to depot" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value t :editor 'boolean-editor)
   (:name 'infeasible-penalty :label "Infeasible penalty" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 1 :max-value 1000000 :default-value 1000000 :editor 'number-editor)
   (:name 'solution-fitness :label "Solution fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 1 :max-value 1000000 :default-value 0 :editor 'number-editor)
   (:name 'min-fitness :label "Min fitness" :accessor-type 'accessor-accessor-type
    :data-type 'number :min-value 0 :max-value 1000000 :default-value 0 :editor 'number-editor)
   (:name 'max-fitness :label "Max fitness" :accessor-type 'accessor-accessor-type
    :data-type 'number :min-value 0 :max-value 1000000 :default-value 200 :editor 'number-editor)
   (:name 'max-capacity :label "Max capacity" :accessor-type 'accessor-accessor-type
    :data-type 'number :min-value 0 :max-value 1000000 :default-value 1600 :editor 'number-editor)
   (:name 'precision :label "Accuracy" :default-value 'float :possible-values '(number double-float float single-float)
    :accessor-type 'accessor-accessor-type :data-type 'symbol :editor 'list-editor)))

(defmethod (setf load-vrp-description-from-file) (file (o entity-vrp-evaluator))
  "Sets the initial <file> string in initial-matrix-file slot of <o>."
  (when file
    (setf (cities-description o) (load-from-file file :tag :cities-description)
          (demand-description o) (load-from-file file :tag :demand-description)
          (max-capacity o) (load-from-file file :tag :max-capacity))))

(defmethod (setf load-vrp-description-from-file) :after (file (o entity-vrp-evaluator))
  (when file
    (setf (max-distance o) (load-from-file file :tag :max-distance))))

(defmethod initialize-fitness-data ((o entity-vrp-evaluator))
  "Initialize <o> fitness data."
  (setf (demand-matrix o) (to-array (demand-description o)))
  (let ((cities-count (cities-count o)))
    (setf (costs-matrix o) (make-array (list cities-count cities-count) :element-type (precision o)))
    (if (euclidean-distance-mode o)
        (initialize-fitness-data-euclidean o))))

(defmethod initialize-fitness-data-euclidean ((o entity-vrp-evaluator))
  (let ((cities-count (cities-count o)))
    (dotimes (i cities-count)
      (dotimes (j cities-count)
        (setf (aref (costs-matrix o) i j) 
              (round (euclidean-distance-between o i j)))))))

(defmethod euclidean-distance-between ((o entity-vrp-evaluator) i j)
  "Answer the cost for travelling from city <i> to city <j> using <o>."
  (let* ((a-coords (nth i (cities-description o)))
         (b-coords (nth j (cities-description o)))
         (a-x (first a-coords))
         (a-y (second a-coords))
         (b-x (first b-coords))
         (b-y (second b-coords)))
    (sqrt (+ (sqr (- b-x a-x))
             (sqr (- b-y a-y))))))

(defmethod euclidean-distance-mode ((o entity-vrp-evaluator))
  (atom (first (demand-description o))))

(defmethod cities-count ((o entity-vrp-evaluator))
  "Answer cities count of <o>."
  (length (cities-description o)))

(defun route-capacity (evaluator route)
  "Answer the <route> total capacity "
  (let ((total 0))
    (dolist (i route)
      (incf total (nth i (demand-description evaluator))))
    total))

(defmethod route-distance ((evaluator entity-vrp-evaluator) route)
  (let ((actual-city 0)
        (vehicle-distance 0))
    (dolist (next-city route)
      (incf vehicle-distance (distance-between evaluator actual-city next-city))
      (setf actual-city next-city))
    ;; Back to depot distance option
    (if (back-to-depot evaluator)
        (incf vehicle-distance (distance-between evaluator actual-city 0)))
    vehicle-distance))

(defmethod distance-between ((o entity-vrp-evaluator) a b)
  (aref (costs-matrix o) a b))

(defmethod evaluate ((evaluator entity-vrp-evaluator) (o entity-sample-vrp))
  "Use <evaluator> to calculate and answer <object> fitness."
  (let ((costs-matrix (costs-matrix evaluator))
        (demand-matrix (demand-matrix evaluator))
        (tour (program o))
        (total-cost 0)
        (max-capacity (max-capacity evaluator)))
    ;; Execute plan
    (dolist (vehicle-plan tour)
      (let ((actual-city 0)
            (vehicle-distance 0)
            (used-capacity 0))
        ;; Tour cost
        (dolist (next-city vehicle-plan)
          (incf vehicle-distance (aref costs-matrix actual-city next-city))
          (incf used-capacity (aref demand-matrix next-city))
          (setf actual-city next-city))
        ;; Back to depot distance option
        (if (back-to-depot evaluator)
            (incf vehicle-distance (aref costs-matrix actual-city 0)))
        ;; Infeasible solution penalty (capacity)
        (if (> used-capacity max-capacity)
            (incf total-cost (infeasible-penalty evaluator)))
        (incf total-cost vehicle-distance)))
    ;; Assign fitness
    (setf (fitness o) total-cost)))

(defmethod evaluate-feasible ((evaluator entity-vrp-evaluator) (o entity-sample-vrp))
  (evaluate-feasible evaluator (program o)))

(defmethod evaluate-feasible-tour ((evaluator entity-vrp-evaluator) tour)
  (let ((costs-matrix (costs-matrix evaluator))
        (demand-matrix (demand-matrix evaluator))
        (max-capacity (max-capacity evaluator)))
    (block evaluate-feasible-tour-block
      ;; Execute plan
      (dolist (vehicle-plan tour)
        (let ((actual-city 0)
              (used-capacity 0))
          ;; Tour cost
          (dolist (next-city vehicle-plan)
            (incf used-capacity (aref demand-matrix next-city))
            (setf actual-city next-city))
          ;; Infeasible solution penalty (capacity)
          (if (> used-capacity max-capacity)
              (return-from evaluate-feasible-tour-block nil))))
      ;; Assign fitness
      t)))

;; #NOTE: Clark and Wright extension
(defmethod is-merge-feasible ((evaluator entity-vrp-evaluator) object i j)
  "Answer whether it´s possible to merge routes <i> and <j> onto <object>."
  (let ((first-route (route-at object i))
        (second-route (route-at object j)))
    (<= (+ (route-capacity evaluator first-route)
           (route-capacity evaluator second-route))
        (max-capacity evaluator))))
