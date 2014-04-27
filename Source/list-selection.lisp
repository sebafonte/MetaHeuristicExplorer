
(defun get-random-element (list)
  "Asnwer a random <list> element."
  (declare (optimize (speed 3) (compilation-speed 0) (safety 0) (debug 0)))
  (let ((n (random-real 0 1)) 
        (sum 0) 
        (operation))
    (dolist (i list)
      (if (<= n sum) (return-from get-random-element operation))
      (setf operation (car i))
      (incf sum (cadr i)))
    operation)) 

(defun get-function-with-max-arguments (list max-arguments)
  "Answer a random function description <list> which <max-arguments> as a constraint."
  (let ((new-list))
    (dolist (i list)
      (setf new-list (append new-list (if (<= (cadr i) max-arguments) (list i)))))
    (nth (random (list-length new-list)) new-list)))
