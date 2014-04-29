
(defclass task ()
  ((name :initarg :name :initform "task" :accessor name)
   (description :initarg :description :accessor description)
   (state :initarg :state :initform 'new :accessor state)
   (input :initarg :input :accessor input)
   (result :initarg :result :accessor result)
   (process :initarg :process :initform nil :accessor process)
   (priority :initarg :priority :initform 10 :accessor priority)
   (log-data :initarg :log-data :accessor log-data)
   (initial-time :initarg :initial-time :initform nil :accessor initial-time)
   (final-time :initform nil :accessor final-time)
   (value :initarg :value :initform 0 :accessor value)))


(defun search-task-running-time (object)
  "Answer the running time for search task <object>."
  (let* ((initial-time (initial-time object))
         (final-time (final-time object))
         (last-time (if (not (zerop final-time)) 
                        final-time 
                      (get-universal-time))))
    (- last-time initial-time)))
  
(defun execute-search (task)
  "Executes the search children of <task>."
  (setf (state task) 'running)
  (execute-task-loop task)
  (setf (state task) 'finished))

(defun execute-task-loop (task)
  "Execute <task> as a process."
  (setf (initial-time task) (get-universal-time))
  (search-loop task)
  (setf (final-time task) (get-universal-time)))

(defun search-loop (task)
  (dotimes (i 100000)
    (let ((p (/ i 1000)))
      (dotimes (j 1000000)
        (* (sin i) (+ (tan (sin j)))))
      (setf (value task) p))))