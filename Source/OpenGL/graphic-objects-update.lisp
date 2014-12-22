
#|
;; Register property examples
(:name 'medium-fitness :label "Fitness 1/2" :accessor-type 'valuable-accessor-type 
 :getter '(lambda (o) (medium-value (population o) #'fitness)) :data-type 'integer 
 :editor 'number-editor :visible nil :category "Result")

;; Log data to build the graphic
(log-data-for-criteria (log-data o) :diversity-measurement)

;; Graphic definition
(make-instance 'graphic-function-r-r
               :subject (selection interface)
               :xmin 0 
               :xmax (max-generations (algorithm (selection interface)))
               :ymin 0 
               :ymax 10
               :valuable-x-list '(lambda (o) (generation o))
               :valuable-y-list '(lambda (o) (second o))
               :datasource-list '(lambda (o) (fitness o)))
|#

(defun menu-inspect-best-individual (interface subtask)
  "Open a new pane-entity-editor with <interface> best individual."
  (check-remote-model-update subtask)
  (when subtask
    (open-editor-with
     interface 
     (make-instance 'object-in-search
                    :object (best-individual subtask)
                    :context subtask))))

(defun menu-inspect-best-individual-current (interface subtask)
  "Open a new pane-entity-editor with <interface> subtask best best individual."
  (declare (ignore subtask))
  (when (selection interface)
    (open-editor-with
     interface 
     (make-instance 'object-reference 
                    :valuable (lambda (task) 
                                (make-instance 
                                 'object-in-search
                                 :object (best-individual task)
                                 :context task))
                    :subject (selection interface)))))
  
(defun menu-current-best-task-individual (interface subtask)
  "Open a new pane-entity-editor with <interface> selected subtask best individual."
  (declare (ignore subtask))
  (when (selection interface)
    (open-editor-with
     interface 
     (multiple-value-bind (best owner-process)
         (best-individual (selection interface))
       (make-instance 'object-reference 
                      :valuable (lambda (task) 
                                  (make-instance 'object-in-search 
                                                 :object (best-individual task)
                                                 :context task))
                      :subject owner-process)))))

(defclass object-reference (object-with-properties)
  ((subject :initarg :subject :accessor subject)
   (valuable :initarg :valuable :accessor valuable)
   (value :initarg :value :accessor value)
   (cached-value :initarg :cached-value :initform nil :accessor cached-value)))


(defmethod referenced-object ((o object-reference))
  (value o))

(defmethod referenced-object ((o t))
  o)

(defmethod update-referenced-value ((o object-reference))
  (setf (value o) (new-value o)))

(defmethod new-value ((o object-reference))
  (funcall (valuable o) (subject o)))

(defmethod initialize-instance :after ((o object-reference) &rest args)
  (update-referenced-value o))

(defparameter menu-description-pane-subtasks
  '(;; Search options
    ("Execution"
     ("Pause" menu-stop-selection lambda-has-selection)
     ("Continue" menu-resume-selection lambda-has-selection)
     ("Delete" menu-delete-selection lambda-has-selection))
     ;; Selected task
     ("Edit"
      ("Save process" menu-save-selection)
      ("Load process template" menu-load-template :disabled)
      ("Save process template" menu-save-template :disabled)
      ("Load default process" menu-load-default)
      ("Save default process" menu-save-default))
     ;; Population inspect
     ("Inspect"
      ("Indviduals"
       ("Best" menu-inspect-best-individual lambda-has-best-individual)
       ("Actual best" menu-inspect-best-individual-current lambda-has-best-individual)
       ("N bests" menu-inspect-best-individuals lambda-has-best-individual)
       ("Principal components" menu-principal-components :disabled)
       ("Fitness correlation" menu-fitness-node-intercorrelation :disabled))
      ;; Search properties
      ("Objects"
       ("Population" menu-view-population lambda-has-selection)
       ("Search algorithm" menu-edit-search-algorithm lambda-has-selection)
       ("Task" menu-edit-search-subtask lambda-has-selection))
      ;; Search graphs
      ("Graphs"
       ("Fitness map" menu-population-fitness-map lambda-has-selection)
       ("Size map" menu-population-size-map lambda-has-selection)
       ("Diversity over time" menu-diversity-vs-time :disabled)
       ("Fitness over time" menu-fitness-vs-time lambda-has-selection)
       ("Mean fitness over time" menu-medium-population-fitness-vs-time lambda-has-selection)
       ("Max size over time" menu-max-size-vs-time lambda-has-selection)
       ("Mean size over time" menu-medium-population-size-vs-time lambda-has-selection)
       ("Property over time" menu-property-vs-time :disabled)
       ("Population graph" menu-population-graph lambda-has-selection)
       ("Operators usage" menu-operator-usage :disabled)
       ("Functions usage" menu-function-usage :disabled)
       ("Constants usage" menu-constant-usage :disabled)
       ("Evaluation time per generation" menu-evaluation-times :disabled)
       ("Best individual evolution" menu-best-individual-evolution :disabled)))
     ;; Other
     ("Operations"
      ("Delete completed" menu-delete-completed)
      ("Delete worst" menu-delete-worser-than))))

(defparameter menu-description-search-tasks
  '(;; Search options
    ("Execution"
     ("Pause" menu-stop-selection lambda-has-selection)
     ("Continue" menu-resume-selection lambda-has-selection)
     ("Delete" menu-delete-selection lambda-has-selection))
    ;; Selected task
    ("Task"
     ("Edit" menu-edit-selection lambda-has-selection)
     ("Processes" menu-open-subtasks-editor lambda-has-selection)
     ("Save task" menu-save-selection lambda-has-selection)
     ("Load task" menu-load-task)
     ("Load task template" menu-load-model)
     ("Save task template" menu-save-model)
     ("Load default task" menu-load-default)
     ("Save default task" menu-save-default))
    ("Other"
     ;; Search tasks
     ("Objects"
      ("Best" menu-best-task-individual lambda-has-best-individual)
      ("Actual best" menu-current-best-task-individual lambda-has-best-individual)
      ("N bests" menu-n-best-task-individuals lambda-has-best-individual))
     ;; Search graphs
     ("Graphs"
      ("Fitness map" menu-population-fitness-map lambda-has-selection)
      ("Size map" menu-population-size-map lambda-has-selection)
      ("Property over time" menu-property-vs-time :disabled)
      ("Operators usage" menu-operator-usage :disabled)
      ("Functions usage" menu-function-usage :disabled)
      ("Constants usage" menu-constant-usage :disabled)))))

;; #NOTE: Extension for editors
(defmethod ephemeral-properties-definition ((o object-reference))
  "Answer <o> ephemeral properties."
  (property-from-values-list
   o
   (:name 'program :label "Program" :accessor-type 'valuable-accessor-type 
    :data-type 'list :editor 'lisp-editor :read-only t :getter 'program-text)
   (:name 'size :label "Size" :accessor-type 'valuable-accessor-type 
    :data-type 'integer :read-only t :editor 'number-editor :getter 'structure-size)
   (:name 'fitness :label "Fitness" :accessor-type 'accessor-accessor-type :read-only t
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 10000 :editor 'number-editor)
   (:name 'object :label "Object" :accessor-type 'accessor-accessor-type :read-only t
    :data-type 'object :editor 'button-editor)
   (:name 'process :label "Context" :accessor-type 'accessor-accessor-type :read-only t
    :data-type 'object :editor 'button-editor)))

(defmethod structure-size ((o object-reference))
  "Answer the gene structure size of <o>."
  (structure-size (value o)))

(defun program-text (o)
  (format nil "~A" (if o (program o))))

;(defmethod apply-changes ((o object-in-search))
;  "Updates <o> for any possible change."
;  (if (context o)
;      (evaluate (algorithm (context o)) (value o))))

(defmethod evolvablep ((o object-reference))
  "Answer whether <o> can be evolved."
  t)

(defmethod drawablep ((o object-reference))
  "Answer whether <o> can be displayed on the GUI."
  t)

(defmethod constant-p ((o object-reference) &optional (check-genotype t) (check-phenotype t))
  "Answers whether <o> is constant."
  (constant-p (value o) check-genotype check-phenotype))

(defmethod program ((o object-reference))
  "Answer <o> program."
  (program (value o)))

(defmethod (setf program) (value (o object-reference))
  "Set <program> to <o>."
  (setf (program (value o)) value))

(defmethod equivalent ((a object-reference) (b object-reference)  
                        &optional &key check-genotype check-phenotype criteria)
  "Answer whether <a> and <b> are equivalent."
  (equivalent (value a) (value b)
               :check-genotype check-genotype
               :check-phenotype check-phenotype
               :criteria criteria))

(defmethod fitness ((o object-reference))
  (fitness (value o)))

(defmethod draw-opengl-on ((o object-reference) canvas viewer)
  "Draws OpenGL scene <o> on <canvas>."
  (draw-opengl-on (value o) canvas viewer))


;; Drawing specific
(defmethod update-referenced-value ((o t))
  nil)

(defun opengl-refresh-interface (interface)
  (when (has-changed (model (pane interface)))
    (update-referenced-value (model (pane interface)))
    (set-model (pane interface) (model (pane interface))))
  (when (graphic-part interface)
    (redisplay-canvas (graphic-part interface))))

(defmethod has-changed ((o t))
  nil)

;; #TODO: Use a 'EQ registry with <o>
(defmethod has-changed ((o object-reference))
  (not (equals-for-has-changed (new-value o) (value o))))

(defmethod equals-for-has-changed ((a object-in-search) (b object-in-search))
  (equal (program (object a))
         (program (object b))))

(defmethod equals-for-has-changed ((a t) (b t))
  (equals a b))

(defmethod set-model ((p pane-editor-entity) o)
  "Set <o> as <p> model."
  (setf (slot-value p 'model) o)
  (reset-image-buffer p)
  (set-model (interface p) (referenced-object o)))

(defmethod set-model :after ((p pane-editor-entity) o)
  "Set <o> as <p> model."
  (declare (ignore o))
  (trigger p :model-changed p))

(defmethod set-model ((p pane-editor-entity-opengl) o)
  "Set <o> as <p> model."
  (setf (slot-value p 'model) o)
  (set-model (interface p) (referenced-object o)))

;; #NOTE: Delete this, remains equal
(defmethod set-model ((p pane-graphic) o)
  "Set <o> as <p> model."
  (setf (model p) o))

#|
;; Stupid object cache
(defmethod has-changed ((o object-reference))
  (has-changed-for o (value o)))

(defmethod has-changed-for ((reference object-reference) (object entity))
  (if (equal (cached-value reference) (program object))
      nil
    (progn 
      (setf (cached-value reference) (program object))
      t)))

(defmethod has-changed-for ((reference object-reference) (object graphic))
  object)

(defmethod has-changed-for ((reference object-reference) (object object-in-search))
  (has-changed-for reference (object object)))
|#
