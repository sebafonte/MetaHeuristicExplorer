
(defclass pattern-simplification (optimization-method)
  ((patterns :initarg :patterns :initform nil :accessor patterns)))


(defun optimize-lisp-math-patterns (method object) 
  "Optimize <object> applying simplification patterns."
  (simplify-patterns (program object) method))

;; #TODO: 
(defun optimize-polynomial-patterns (method object) 
  "Optimize <object> applying simplification patterns."
  (block nil
    (progn 
      (dolist (i terms-count)
        (let ((new-program (delete-polynomial-term object i)))
          (setf (program new-object) new-program)
          nil)))))

(defun delete-polynomial-term (object term-index)
  "Answer a polynomial as a copy of <object> without term indexed by <term-index>."
  object)
