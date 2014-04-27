;;; Print functions for tasks, population and some other for REPL usage and text files

(defmethod print-state ((p population))
  "Print <p> state info."
  (let ((population (individuals-array p))
        (size-total 0) 
        (fitness-total 0))
    (dotimes (i (length population))
      (let ((size (tree-size (program (aref population i))))
            (fitness (fitness (aref population i))))
        (incf size-total size) 
        (incf fitness-total fitness)
        (format t "Index: ~A  Size: ~A  Fitness: ~A -  Exp: ~A~%" i size (float fitness) 
                (program (aref population i)))))
    (format t "~%~%Average size: ~A ~%Average fitness: ~A~%" 
            (float (/ size-total (length population))) 
            (float (/ fitness-total (length population))))))
			
;; #TODO: Add option to print population
(defun print-subtasks (&optional &key print-population)
  "Print sysem subtasks state info."
  (declare (ignore print-population))
  (dolist (p *search-subtasks*)
    (print-state p)))

(defun print-subtasks-result (&optional &key print-expression)
  "Print sysem subtasks result info."
  (if *search-subtasks*
    (let ((best-of-all)
          (best)
          (good 0))
      (dolist (p *search-subtasks*)
        (setf best (best-individual (algorithm p)))
        (if (>= (fitness best) (solution-fitness (algorithm p))) (incf good))
        (if (or (not best-of-all) (< (fitness best-of-all) (fitness best)))
            (setf best-of-all best))
        ;; Print subtask result
        (format t "Name: ~A   - Generation: ~A     Fitness: ~A -   ~A~%"
                (name p)
                (generation (algorithm p))
                (fitness (best-individual (algorithm p)))
                (if print-expression (program best) "")))
      ;; Print subtasks result (general)
      (format t "~%Subtasks: ~A (~A good)~%Best fitness: ~A -   ~A~%~%"
              (length *search-subtasks*)
              good
              (program best-of-all)
              (fitness best-of-all)))))

(defun print-subtasks-fitness ()
  "Print sysem subtasks fitness info."
  (print "")
  (dolist (i *search-subtasks*)
    (if (>= (fitness (best-individual (algorithm i)))  
           (solution-fitness (algorithm i))) 
        (print (program (best-individual (algorithm i)))))))
