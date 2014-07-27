
(defclass task-benchmark (base-benchmark)
  ())


(defmethod prepare-benchmark-task-callback ((o task-benchmark) subject)
  "Connect <task> events and initialize <task> data dictionary."
  (dolist (i (children subject))
    (setf (log-inspectors o) nil)
    (initialize-log-inspectors o i)
    (inspect-object o)
    (setf (benchmarker i) o)))

(defmethod prepare-benchmark ((o task-benchmark) subject)
  "Connect <task> events and initialize <task> data dictionary."
  (when-send-to subject :task-initialized 'prepare-benchmark-task-callback o subject)
  (setf (benchmarker subject) o))

(defmethod initialize-properties :after ((o task-benchmark))
  "Initialize <object> properties.
   #NOTE: these properties should return the function to obatin the value for the argument (a task)."
  (add-properties-from-values
   o
   ;; Fitness
   (:name 'best-fitness :label "Best fitness" :accessor-type 'property-accessor-type
    :data-type 'number :default-value (lambda (object) (fitness (best-individual object))))
   ;; Time   
   (:name 'time :label "Time" :accessor-type 'property-accessor-type
    :data-type 'number :default-value (lambda (object) (get-value-for-property-named object 'running-time)))
   ;; Performance measures
   (:name 'likelihood-of-optimality :label "Optimality" :accessor-type 'property-accessor-type
    :data-type 'number :default-value (lambda (object) (lambda-likelihood-of-optimality o object)))
   (:name 'average-fitness-value :label "Average best fitness" :accessor-type 'property-accessor-type
    :data-type 'number :default-value (lambda (object) (lambda-average-fitness-value o object)))
   (:name 'likelihood-of-evolution-leap :label "Evolution Leap" :accessor-type 'property-accessor-type
    :data-type 'number :default-value (lambda (object) (lambda-likelihood-of-evolution-leap o object)))
   (:name 'evaluations :label "Evaluations" :accessor-type 'property-accessor-type
    :data-type 'number :default-value (lambda (object) (reduce '+ (mapcar (lambda (o) (evaluations (fitness-evaluator o)))
                                                                          (children object)))))))

(defmethod initialize-log-inspectors :after ((o task-benchmark) subject)
  (appendf (log-inspectors o)
           (list 
            (make-instance 'log-inspector
                            :name 'inspector-best-individual-by-generation
                           :event :progress-change
                           :subject (algorithm subject)
                           :action (lambda (log-inspector &rest args)
                                     (declare (ignore args))
                                     (let ((algorithm (subject log-inspector))
                                           (task (context (subject log-inspector))))
                                       (save-log-data-set
                                        o
                                        :generation (generation algorithm)
                                        :best-individual (best-individual task)
                                        :medium-fitness (get-value-for-property-named task 'medium-fitness)
                                        :medium-size (get-value-for-property-named task 'medium-size))))))))

(defun lambda-likelihood-of-optimality (benchmark object)
  "Likelihood of optimality benchmark measure."
  (declare (ignore benchmark))
  (count-if
   (lambda (o) 
     (let ((best-individual (best-individual o))
           (solution-fitness (solution-fitness (fitness-evaluator o))))
       (or (better-than-fitness-value best-individual solution-fitness)
           (= (fitness best-individual) solution-fitness))))
   (children object)))

;; #NOTE: Evaluated when all children have finished
(defun lambda-average-fitness-value (benchmark object)
  "Average fitness value of the best individuals found."
  (declare (ignore benchmark))
  (let ((values (mapcar (lambda (o) (fitness (best-individual o))) (children object))))
    (/ (reduce '+ values) (length values))))

(defun lambda-likelihood-of-evolution-leap (benchmark object)
  "Likelihood of evolution leap benchmark measure."
  (let ((all (unique-equals 
              (mapcar (lambda (o) (program (second (assoc :best-individual (data o)))))
                      (log-data-for-criteria (log-data benchmark) :best-individual)))))
    (/ (reduce '+ (mapcar 'fitness all))
       (length (children object)))))

