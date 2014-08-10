
(defclass search-task (entity object-with-properties)
  (;; Basic properties
   (name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (state :initarg :state :initform 'new :accessor state)
   (children :initarg :children :accessor children)
   ;; Properties as a subtask 
   (input :initarg :input :accessor input)
   (result :initarg :result :accessor result)
   (process :initarg :process :initform nil :accessor process)
   (priority :initarg :priority :accessor priority)
   (random-seed :initarg :random-seed :accessor random-seed)
   (seed :initarg :seed :initform nil :accessor seed)
   ;; Benchmark
   (benchmarker :initarg :benchmarker :accessor benchmarker)
   (initial-time :initarg :initial-time :initform nil :accessor initial-time)
   (final-time :initform nil :accessor final-time)
   ;; Objetive
   (objetive-class :initarg :objetive-class :accessor objetive-class)
   (fitness-evaluator :initarg :fitness-evaluator :accessor fitness-evaluator)
   (language :initarg :language :accessor language)
   (algorithm :initarg :algorithm :accessor algorithm)
   ;; Planification
   (task-builder :initarg :task-builder :accessor task-builder)
   (task-planifier :initarg :task-planifier :accessor task-planifier)))


(defmethod initialize-instance :after ((object search-task) &rest initargs)
  "Initialize <object>."
  (unless (find :forget-defaults initargs)
    (initialize-algorithm object)))

(defmethod log-data ((o search-task))
  (log-data (benchmarker o)))

(defmethod (setf algorithm) (value (object search-task))
  (setf (slot-value object 'algorithm) value)
  (initialize-algorithm object))

(defmethod initialize-algorithm ((object search-task))
  "Initialize <object> default algorithm."
  (setf (context (algorithm object)) object))

(defmethod initialize-properties :after ((task search-task))
  "Initialize <task> properties."
  (let ((objetive-class (default-search-object-class task))
        (default-algorithms (default-search-algorithms)))
    (add-properties-from-values
     task
     ;; Default properties
     (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
      :data-type 'string :default-value "New task" :editor 'text-editor)
     (:name 'description :label "Description" :accessor-type 'accessor-accessor-type 
      :data-type 'string :default-value "New task" :editor 'text-editor)
     (:name 'priority :label "Priority" :accessor-type 'accessor-accessor-type 
      :data-type 'integer :editor 'number-editor :default-value *default-search-task-process-priority*
      :min-value *default-search-task-min-process-priority* :max-value *default-search-task-max-process-priority*
      :object-parameter t)
     (:name 'state :label "State" :accessor-type 'accessor-accessor-type :data-type 'symbol
      :default-value 'stopped :editor 'text-editor :read-only t)
     (:name 'random-seed :label "Random seed" :accessor-type 'accessor-accessor-type 
      :data-type 'boolean :default-value t :editor 'boolean-editor)
     (:name 'seed :label "Seed" :accessor-type 'accessor-accessor-type :object-parameter t
      :data-type 'integer :min-value 0 :max-value 2000000000 :default-value 2)
     ;; Default composition properties
     (:name 'task-planifier :label "Planifier" :accessor-type 'accessor-accessor-type 
      :editor 'configurable-copy-list-editor :data-type 'object
      :default-value (default-local-only-task-planifier) :possible-values (system-global-task-planifiers))
     (:name 'task-builder :label "Builder" :accessor-type 'accessor-accessor-type :editor 'button-editor 
      :default-value (make-instance 'n-runs-task-builder))
     (:name 'result :label "Result" :accessor-type 'accessor-accessor-type :editor 'button-editor :category "Result")
     ;; Objetive
     (:name 'objetive-class :label "Objetive class" :accessor-type 'accessor-accessor-type
      :data-type 'symbol :default-value objetive-class :possible-values (possible-classes-to-search) 
      :editor 'list-editor :category "Objetive")
     (:name 'algorithm :label "Search algorithm" :accessor-type 'accessor-accessor-type 
      :data-type 'model :possible-values default-algorithms :default-value (copy-cyclic (first default-algorithms))
      :editor 'configurable-copy-list-editor :setter '(setf algorithm))
     ;; Children
     (:name 'children :label "Children" :accessor-type 'accessor-accessor-type :editor 'list-editor :visible nil)
     ;; Execution information
     (:name 'benchmarker :label "Log data" :accessor-type 'accessor-accessor-type :editor 'button-editor :category "Result")
     (:name 'best-fitness :label "Fitness" :accessor-type 'valuable-accessor-type :read-only t
      :getter '(lambda (task) 
                 (if (best-individual task)
                     (my-round-to-3 (fitness (best-individual task)))
                   0))
      :category "Result")
     (:name 'best-size :label "Best size" :accessor-type 'valuable-accessor-type 
      :getter '(lambda (object)  
                 (if (best-individual object) 
                     (structure-size (best-individual object))))
      :data-type 'integer :editor 'lisp-editor :visible t :read-only t :category "Result")
     (:name 'medium-fitness :label "Fitness 1/2" :accessor-type 'valuable-accessor-type 
      :getter '(lambda (object) (medium-value (population object) #'fitness)) :data-type 'integer 
      :editor 'number-editor :visible nil :category "Result")
     (:name 'medium-size :label "Size 1/2" :accessor-type 'valuable-accessor-type 
      :getter '(lambda (object) (medium-value (population object) #'structure-size))
      :data-type 'number :editor 'lisp-editor :visible nil :category "Result")
     (:name 'progress-indicator :label "Progress" :accessor-type 'valuable-accessor-type :read-only t
      :getter '(lambda (task) 
                 (if (children task)
                     (* 100
                        (/ (length (select (children task) (lambda (task) (is-completed task))))
                           (length (children task))))
                   (default-progress-indicator-value (algorithm task))))
      :category "Result")
     (:name 'initial-time :label "Initial time" :accessor-type 'valuable-accessor-type 
      :getter '(lambda (task)
                 (if (children task)
                     (reduce 'min (mapcar (lambda (task) (if (initial-time task) (initial-time task) 0))
                                          (children task)))
                   0))
      :data-type 'integer :editor 'number-editor :visible nil :read-only t :category "Result")
     (:name 'final-time :label "Final time" :accessor-type 'valuable-accessor-type 
      :getter '(lambda (task)
                 (let ((not-nulls (select (children task) (lambda (o) (not (null (final-time o)))))))
                   (if not-nulls
                       (reduce 'max (mapcar 'final-time not-nulls)))))
      :data-type 'integer :editor 'number-editor :visible nil :read-only t :category "Result")
     (:name 'running-time :label "Time" :accessor-type 'valuable-accessor-type 
      :getter '(lambda (object) (max (task-running-time object) 0))
      :read-only t :data-type 'integer :category "Result")
     ;; Dependent properties
     (:name 'language :label "Language" :accessor-type 'accessor-accessor-type :category "Objetive"
      :data-type 'model :editor 'configurable-copy-list-editor 
      :dependency (make-eql-language-dependence 'objetive-class)
      :default-value-function (lambda (objetive-class) (copy-cyclic (default-language (make-instance objetive-class))))
      :possible-values-function (lambda (objetive-class) (possible-languages (make-instance objetive-class))))
     (:name 'fitness-evaluator :label "Fitness evaluator" :accessor-type 'accessor-accessor-type 
      :editor 'configurable-copy-list-editor :category "Objetive" :data-type 'model
      :dependency (make-possible-class-dependency 'objetive-class)
      :default-value-function (lambda (objetive-class) (copy-cyclic (first (default-fitness-evaluators (make-instance objetive-class)))))
      :possible-values-function (lambda (objetive-class) (default-fitness-evaluators (make-instance objetive-class)))))))

(defun task-running-time (task)
  "Answer the running time for <task>."
  (let* ((initial-time (get-value-for-property-named task 'initial-time))
         (final-time (get-value-for-property-named task 'final-time)))
    (if (null final-time) 
        (if (null initial-time)
            0
          (- (get-universal-time) initial-time))
      (- final-time initial-time))))

(defmethod execute-search ((task search-task))
  "Executes the search children of <task>.
   Initialization and getting results from task's children is achieved."
  (setf (state task) 'initialization)
  (set-children-initial-data task)
  (trigger task :task-initialized)
  (setf (state task) 'running)
  (execute-planified-search task)
  (setf (state task) 'finished))

(defmethod execute-planified-search ((task search-task))
  "Execute children of <task> using it's asociated planiifer.
   #NOTE / #TODO: Now is using lispworks planification."
  (execute-task-subtasks (task-planifier task) task)
  (mp:process-wait 
   "Waiting for children completion."
   (lambda (children)
     (reduce (lambda (x y) (and x y))
             (mapcar (lambda (subtask) (is-completed subtask))
                     children)))
   (children task)))

(defmethod execute-subtask-loop ((task search-task))
  "Execute <task> as a process."
  (setf (context (algorithm task)) task)
  (prepare-to-search task)
  (setf (initial-time task) (get-universal-time))
  (search-loop (algorithm task) (seed task))
  (setf (final-time task) (get-universal-time))
  (reset-temporary-data (fitness-evaluator task)))

(defmethod prepare-to-search ((task search-task))
  "Prepares <task> to be executed."
  (let ((evaluator (fitness-evaluator task)))
    (initialize-fitness-data evaluator)
    (initialize-subtask-seed task)
    (specialize-language task evaluator)))

(defmethod initialize-subtask-seed ((task search-task))
  "Initialize <task> random seed when needed."
  (when (random-seed task) 
    (setf (seed task) (random-integer 1 1000000))))

(defmethod set-children-initial-data ((task search-task))
  "Redefined by subclasses."
  (build (task-builder task) task)
  (dolist (subtask (children task))
    (set-task-values-into task subtask)))

(defmethod default-fitness-evaluators ((o search-task))
  "Answer the default classes that can evaluate <o> fitness."
  (declare (ignore o))  
  (list 
   (system-get 'default-search-task-objetive-fitness-evaluator)))

(defmethod default-population-initializer ((o search-task))
  "Answer the default population initializer for <o>."
  (declare (ignore o))
  (system-get 'sample-property-sampling-initializer))

(defmethod default-search-object-class (o)
  "Answer <o> default search class."
  (declare (ignore o))
  'entity-function-x)

(defmethod possible-languages ((o search-task))
  (list 
   (system-get 'search-task-default-language)))

(defmethod prepare-children-from ((o search-task) children algorithm)
  "Prepares <o> to behave like <children>."
  (declare (ignore algorithm))
  (setf (program o) children))

;; #DEPENDENCY HOOK
(defun lambda-update-callback-search-task (object property) 
  (declare (ignore property))
  nil)

(defmethod ejecutar-wait ((task search-task))
  "Stop <task>."
  (mp:process-stop (process task))
  (setf (state task) 'STOPPED))

(defmethod ejecutar-signal ((task search-task))
  "Signal <task>."
  (mp:process-unstop (process task))
  (setf (state task) 'RUNNING))

(defmethod kill-task ((task search-task))
  "Kills <task>."
  ;; Kill task mp:process 
  (when (process task)
    (mp:process-kill (process task)))
  ;; Kill subtasks mp:process
  (dolist (subtask (children task))
    (kill-task subtask)))

(defmethod resetear ((task search-task))
  "Reset <task>."
  (resetear (algorithm task))
  (mp:process-reset (process task)))

(defmethod is-completed ((task search-task))
  "Answer whether <task> as been executed."
  (equal 'FINISHED (state task)))

;; #TODO: Refactor to some kind of copy ?
(defmethod set-task-values-into ((o search-task) (p search-task))
  (setf (slot-value p 'fitness-evaluator) (copy (fitness-evaluator o))
        (slot-value p 'objetive-class) (objetive-class o)
        (slot-value p 'language) (language o)))

;; #TODO: Completed subtasks progress if it has. If not, task progress (for example)
(defmethod progress-indicator ((o search-task))
  "Answer the value for <task> progress indicator."
  0)

(defmethod extract-process-output-data ((o search-task))
  "Process running results of <task>."
  nil)

(defmethod continue-execution ((o search-task))
  "Continue the execution of <task>."
  nil)

;; #TODO: Refactor / delete this generic function
(defmethod evolvablep ((o search-task))
  "Answer whether <o> can be evolved."
  nil)

(defmethod drawablep ((o search-task))
  t)

(defmethod compute-object-interface-pixmap-step ((o search-task) subtask pixmap width heigth render-precision)
  "Computes pixel values into <pixmap> of <o>."
  nil)

(defmethod draw-in-pixmap (pinboard object pane (o search-task) parent-pinboard x y)
  "Draws object in the pixmap of pinboard interface."
  (not-available-pixmap pinboard object pane o parent-pinboard x y))

(defmethod individuals ((o search-task))
  "Answer <object> individuals without nils."
  (individuals (population o)))

(defmethod best-individual ((task search-task))
  "Anwer the best individual found by <task>."
  (if (children task)
      (best-individual-subtasks task)
    (best-individual (algorithm task))))

(defmethod best-individual-subtasks ((task search-task))
  "Answer the best individual and the owner subtask in <task> subtasks."
  (let ((best-individual)
        (owner-subtask))
    (dolist (subtask (children task))
      (let ((best-individual-subtask (if (algorithm subtask) (best-individual (algorithm subtask)))))
        (if (or (null best-individual)
                (and best-individual-subtask
                     (better-than best-individual-subtask best-individual)))
            (setf best-individual best-individual-subtask
                  owner-subtask subtask))))
      (values best-individual owner-subtask)))

(defmethod best-individuals ((task search-task) n)
  "Answer <n> best individuals of <task>."
  (let ((population (make-instance 'population))
        (elites))
    (dolist (subtask (children task))
      (if (population subtask)
          (appendf elites (individuals subtask))))
    (setf (individuals-array population) (to-array elites))
    (best-individuals population n)))

(defmethod population ((task search-task))
  "Answer <task> population."
  (if (children task)
      (subtasks-population task)
    (population (algorithm task))))

(defmethod subtasks-population ((task search-task))
  "Answer a new population with all <task> subtasks populations contents."
  (let ((elites)
        (population (make-instance 'population)))
    (dolist (subtask (children task))
      (when (population subtask)
        (appendf elites (individuals subtask))))
    (setf (individuals-array population) (to-array elites))
    population))

(defmethod max-size ((task search-task))
  "Answer <task> max size."
  (if (children task)
      (max-size-subtasks task)
    (max-size (algorithm task))))

(defmethod max-size-subtasks ((task search-task))
  "Answer <task> subtasks max size."
  (reduce 'max 
          (mapcar (lambda (p) (max-size p)) 
                  (select 
                   (children task)
                   (lambda (o) (not (null o)))))))

(defmethod possible-initialization-methods-for ((o search-task))
  (list 
   (system-get 'random-trees-cfg-initializer)))