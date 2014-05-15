
(defclass n-runs-task-builder (task-builder)
  ((runs :initarg :runs :accessor runs)))


(defmethod initialize-properties :after ((object n-runs-task-builder))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :default-value "N runs task builder" :editor 'text-editor)
   (:name 'runs :label "Runs" :accessor-type 'accessor-accessor-type :object-parameter t
    :data-type 'integer :min-value 1 :max-value 1000000 :default-value 1 :editor 'number-editor)))

(defmethod build ((builder n-runs-task-builder) (task search-task))
  "Creates a task with <builder>."
  (setf (children task) (search-subtasks builder task)))

(defmethod search-subtasks ((builder n-runs-task-builder) (task search-task))
  "Answer the search subtasks for <builder>."
  (let ((subtask-list)
        (old-process (process task)))
    (setf (process task) nil)
    (dotimes (i (runs builder))
      (appendf subtask-list (list (copy-cyclic task))))
    (setf (process task) old-process)
    subtask-list))

(defmethod print-object ((builder n-runs-task-builder) seq)
  (format seq "~A (~Ax)" (name builder) (runs builder)))