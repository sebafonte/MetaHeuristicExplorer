
;; #TODO: Pass second #'simplify (strategy) argument correctly
(defun optimize-lisp-math-simplification (method object)
  "Optimize <object> applying constants expresions simplification."
  (declare (ignore method))
  (simplify-strategy (object object) nil (algorithm (context object))))

;; #TODO: 
(defun optimize-round-constants (method object)
  "Optimize <object> applying constant rounding."
  (declare (ignore method))
  (let ((list (constant-indexes object))
        (delta-value 0.1))
    (dolist (i list)
      (if (> (abs (- i (round i))) delta-value)
          (replace-indexed-variable i (round i))))
    (setf (program object) new-program)
    object))

(defun constant-indexes (object)
  nil)
      
(defun replace-indexed-variable (variable-descriptor new-value)
  nil)

(defun optimize-polynomial-delete-term (method object)
  "Optimize <object> deleting unnecesary terms."
  (declare (ignore method))
  (let* ((program (program (object object)))
         (best-individual object)
         (terms-count (length program)))
    nil))

(defun optimize-vrp-2-opt (method object)
  "Optimize <object> using 2-opt heuristic."
  nil)

(defun optimize-vrp-3-opt (method object)
  "Optimize <object> using 3-opt heuristic."
  nil)

(defun optimize-lop-list-compact (method object)
  "Optimize <object> cleaning it's permutations list."
  nil)
