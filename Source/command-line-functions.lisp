
(defclass command-line-interpreter ()
  ((name :initarg :name :accessor name)))

(defclass task-group ()
  ((name :initarg :name :accessor name)
   (tasks :initarg :tasks :accessor tasks)))


(defmethod apply-command-line-parameters ((c command-line-interpreter))
  (let* ((command-line system:*line-arguments-list*)
         (exit-argument (argument-from-key-equal command-line ":exit" 1)))
    (apply-command c command-line)
    (when (and exit-argument (eval (read-from-string exit-argument)))
      (force-exit-image))))

(defmethod apply-command ((c command-line-interpreter) command-line)
  "Execute commands in <command-line>."
  (let ((command-file (argument-from-key-equal command-line ":command-file" 1)))
    (when command-file
      ;; #TODO: Replace this global binding to an environment argument
      (setf *command-directory* (pathname-location command-file))
      (let* ((command-string (load-commands-from-file command-file))
             (command-lines (mapcar (lambda (object) 
                                      (let ((trimed (string-trim " " object)))
                                        (if (and (not (equal trimed ""))
                                                 (not (equal (aref trimed 0) #\;)))
                                            (read-from-string object))))
                                    command-string)))
        (execute-command-line c command-lines)))))

(defmethod execute-command-line ((c command-line-interpreter) lines)
  (dolist (line lines)
    (execute-operation c line)))

(defmethod execute-operation ((c command-line-interpreter) operation)
  "Executes operation described in <string>."
  (eval operation))

(defmethod load-commands-from-file (file-path &optional &key tag)
  (let ((return-list))
    (with-open-file (stream file-path)
      (do ((line (read-line stream) (read-line stream nil 'eof)))
          ((eq line 'eof) "Reached end of file.")
        (appendf return-list (list line))))
    return-list))

;;; Operations
;; Task loading
(defun register-task (name task)
  "Register <task> in the sistem with <name>."
  (let ((new-task (eval task)))
    (setf (name new-task) name)
    (system-add new-task)))

(defun register-task-path (name path)
  "Register <task> in the sistem with <name>."
  (let ((new-task (load-object-from path)))
    (when name (setf (name new-task) name))
    (system-add new-task)))

(defun register-task-group (name &rest group-description)
  "Register a task group from <group-description with <name>."
  (let ((new-group))
    (system-add-with-name new-group name)))

(defun register-task-group-path (name &key path)
  "Register a task group from <group-description with <name>."
  (let ((new-group (load-object-from path)))
    (system-add-with-name new-group name)))

;; Task set / iterators creation
(defun register-group-of-range (group-name task-name setter from-value to-value step)
  (let ((tasks)
        (task (system-get task-name))
        (index 0))
    (loop for value from from-value to to-value by step do
          (incf index)
          (let ((new-task (copy-cyclic task)))
            (setf (name new-task) (format nil "~A-~A" (name new-task) index))
            (apply (eval setter) (list task value))
            (appendf tasks (list new-task))))
    (system-add (make-instance 'task-group :name group-name :tasks tasks))))

(defun register-group-of-list (group-name task-name setter list-of-values step)
  (let ((tasks)
        (task (system-get task-name))
        (index 0))
    (dolist (value list)
      (let ((new-task (copy-cyclic task)))
        (incf index)
        (setf (name new-task) (format nil "~A-~A" (name new-task) index))
        (apply (eval setter) (list task value))
        (appendf tasks (list new-task))))
    (system-add (make-instance 'task-group :name group-name :tasks tasks))))

;; Task saving
(defun save-task-path (name path)
  "Save task registered as <name> in <path>."
  (save-source-description (system-get name) path))

(defun save-task-group (name path)
  "Save task group registered as <name> in <path>."
  (save-source-description (system-get name) path))

;; Task execution
(defun execute-registered-task (name)
  "Executes task named <name> saving it result (or search task final state)."
  (let ((task (system-get name)))
    (execute-until-finished task "Executing registered task...")))

(defun execute-registered-task-group (name)
  "Executes task named <name> saving it result (or search task final state)."
  (dolist (task (system-get name))
      (execute-task (task-planifier task) task)))

(defun execute-registered-group (group-name)
  (dolist (task (tasks (system-get group-name)))
    (execute-until-finished task "Executing task from group...")))

(defmethod execute-until-finished (task description)
  (execute-task (task-planifier task) task)
  (mp:process-wait 
   description
   (lambda (object) 
     (declare (ignore object))
     (equal (state task) 'finished))
   task))

;; Results reporting
(defun report-results-name (&key name format path mode options)
  "Save result of task named <name> into <path>."
  (report-results :object (system-get name) :format format :path path :mode mode :options options))

(defun report-results (&key object format path mode options)
  "Save result of task named <name> into <path>."
  (declare (ignore mode) (ignore format))
  (report-on-file object path options))

;; Environment
(defmethod set-distributed-environment-configuration ((c command-line-interpreter) &key path)
  "Evaluates <expression> loaded from the command line."
  (let ((task (system-get name)))
    (execute-task (task-planifier task) task)))

(defmethod load-panes ((c command-line-interpreter) &key path)
  "Load default pane on main interface from <path>."
  (load-object-from path))

;; Other auxiliar
(defmethod eval-expression ((c command-line-interpreter) expression)
  "Evaluates <expression> loadded from the command line."
  (eval expression))

(defmethod set-no-gui ((c command-line-interpreter))
  "Set the parameter to avoid opening main interface on startup."
  (setf *open-gui-on-startup* nil))

(defun force-exit-image ()
  "Force exit lisp image."
  (lispworks:quit))
