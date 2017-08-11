
(defun start-task-function (task)
  (format nil "Started")
  (execute-search task)
  (print "Ended"))

(defun kill-task-function (task)
  (kill-task task)
  (print "Killed"))

(defun run-timed-task (task time)
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


(defun start-block-function (function)
  (format nil "Started")
  (let ((value (funcall function)))
    (print "Ended")
    value))

(defun kill-block-function (process)
  (mp:process-kill process)
  (print "Killed"))


(defun run-timed-block (function time)
  (let* ((started)
         (ended)
         (result)
         (start-function (lambda () 
                            (setf 
                             started t
                             result (start-block-function function)
                             ended t))))
    (let ((process (mp:process-run-function "Running the function" nil start-function)))
      (let* ((kill-function (lambda () 
                              (kill-block-function process)
                              (setf ended t)))
             (timer (mp:make-timer 'mp:process-run-function "Running the timer" () kill-function)))
      (mp:schedule-timer-relative-milliseconds timer time)
      (mp:process-wait "Running wait" (lambda () (and started ended)))))
    result))



;(defun run-timed-problem (problem time)
;  (run-timed-task (create-task-for-problem problem) time))

;(defun create-task-for-problem ()
;  (make-instance 'search-task))

;(time (test-timed-task nil 500))

#|
CL-USER 5 : 1 > (run-timed-block (lambda () (dotimes (i 1000000) (*  i i i 1.3123123)) 77) 15000)
NIL

CL-USER 6 : 1 > (run-timed-block (lambda () (dotimes (i 1000000) (*  i i i 1.3123123)) 77) 15000)
77
|#