
(defun start-task-function (task)
  (format nil "Started")
  (execute-search task)
  (print "Ended"))

(defun kill-task-function (task)
  (kill-task task)
  (print "Killed"))

(defun run-timed-task (task time)
  (declare (ignore problem))
  (let* ((kill-function (lambda () (kill-task-function task)))
         (start-function (lambda () (start-task-function task)))
         (timer (mp:make-timer 'mp:process-run-function "Running the timer" () kill-function)))
    (mp:schedule-timer-relative-milliseconds timer time)
    (let ((process (mp:process-run-function "Running the function" nil start-function)))
      (mp:process-wait "Running wait" (lambda () (or (eql (state task) 'FINISHED) (eql (state task) 'KILLED))))
      (mp:process-kill process))
    (values 
     (if (best-individual task)
         (program (best-individual task))
       (best-individual task))
     task)))

(defun run-timed-problem (problem time)
  (run-timed-task (create-task-for-problem problem) time))

(defun create-task-for-problem ()
  (make-instance 'search-task))


;(time (test-timed-task nil 500))