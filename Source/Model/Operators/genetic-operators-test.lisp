
(defmethod n-branch-delete (exp (a search-algorithm))
  "Delete multiple subtrees from <exp> while fitness does not decrease."
  (let* ((program)
         (best exp)
         (gen (make-instance 'genotype :expresion exp))
         (fitness-best (evaluate a gen))
         (fitness-program)
         ;; #TODO: Un-hardcode this :)
         (amount 5))
    (dotimes (i amount)
      (setf program (operate (system-get 'branch-delete) a (list best))
            (expresion gen) program)
      ;; If it's better and smaller, take the new as the best and store fitness
      (when (and (>= fitness-best (setf fitness-program (evaluate a gen)))
                 (< (tree-size program) (tree-size best)))
        (setf best program
              fitness-best fitness-program)))
    best))


;; #TODO: Subtree replacement:
;;           - Function to replace a tree with a subtree in each node (Done)
;;           - Function to replace a tree with a subtree in certain node combinations (#TODO)
;; 
;;        #TEST: (inject-tree-with-subtree '(+ (* x x) (* y y) (* z z)) '(sin x))
;;               (inject-tree-with-subtree '(+ x (* x x) (* x x x) (* x x x x)) '(sin x))
;;               (inject-tree-with-subtree-combinations ...)
;;               (inject-tree-with-subtree-combinations ...)
;;
(defun inject-tree-with-subtree (tree subtree algorithm)
  "Replace each node of <tree> with <subtree>, evaluate and print fitness value."
  (let ((gen (make-instance 'genotype))
        (object (make-instance (objetive-class algorithm))))
    (dotimes (i (count-internal-subtrees tree algorithm))
      (let ((tree (replace-internal-subtree tree subtree (1+ i) (language algorithm))))
        (format t "~A  Fitness: ~A ~%"
                (expresion (setf (expresion gen) tree
                                 (gen object) gen))
                (float (evaluate algorithm object)))))))

(defun relevant-nodes-replacing-subtrees (tree subtrees algorithm n)
  "Answer the structure which produces best fitness improvements."
  (declare (ignore n))
  (let* ((gen (make-instance 'genotype :expresion tree))
         (object (make-instance (objetive-class algorithm) :gen gen))
         (fitness-original (evaluate algorithm object))
         (current-fitness)
         (result))
    (dotimes (i (count-internal-subtrees tree algorithm))
      (let ((sum 0))
        (dolist (subtree subtrees)
          (setf tree (replace-internal-subtree tree subtree (1+ i) (language algorithm))
                (expresion gen) tree
                (gen object) gen
                current-fitness (evaluate algorithm object))
          (incf sum (abs (- fitness-original current-fitness)))
          (format t "-- ~A  Fitness: ~A ~%" (expresion gen) (float current-fitness)))
        (format t "--------- ~A  Sum: ~A ~%" (expresion gen) (float sum))
        (push (list (expresion gen) i sum) result))
      result)))

;; #TODO: Not working, check
(defun inject-tree-with-subtree-combinations (tree subtree n)
  "Replace <tree> with <subtree> in certain node combinations and print result."
  (dolist (i (replacement-indexes tree n))
    (let ((tree-combination (copy-tree tree)))
      (dolist (j i)
        (setf tree-combination (replace-subtree tree-combination subtree (1+ j))))
      (print tree-combination))))

;; #TODO: For test purposes only, this should return al possible combinations
(defun replacement-indexes (tree n)
  "Answer a list with possible node indexes for replacement."
  '((1 2) (2 3) (3 4)))

