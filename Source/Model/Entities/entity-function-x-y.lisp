(defclass entity-function-x-y (entity-function)
  ())


(defmethod compiled-program ((o entity-function-x-y))
  "Answer the compiled function that representing o genotype."
  (compile nil `(lambda () 
                  (declare (special x) (special y))
                  ,(program o))))

(defmethod constant-p ((o entity-function-x-y) &optional (check-genotype t) (check-phenotype t))
  "Answers whether <o> is constant."
  (block 1
    ;; Check genotype
    (if (and check-genotype (subexp-constant-p (program o) o))
      (return-from 1 t))
    ;; Check phenotype
    (if check-phenotype
        (let* ((pixels-x (pixels-x o))
               (pixels-y (pixels-y o))
               (delta-x (/ (heigth o) pixels-x))
               (delta-y (/ (width o) pixels-y))
               (function (compiled-program o))
               (start-x (start-position-x o))
               (start-y (start-position-y o))
               (first-value)
               (x)
               (y))
          (declare (special x) (special y))
          (block 1
            (dotimes (i pixels-x)
              (dotimes (j pixels-y)
                (setf x (+ start-x (* i delta-x))
                      y (+ start-y (* j delta-y)))
                (if first-value
                    (if (not (equals (vec-crop 0 1 (funcall function)) first-value))
                        (return-from 1 nil))
                  (setf first-value (vec-crop 0 1 (funcall function))))))
            t))
      nil)))

(defmethod default-language ((o entity-function-x-y))
  (system-get 'lisp-math-function-xy))

(defmethod possible-languages ((o entity-function-x-y))
  (list 
   (system-get 'lisp-math-function-xy)
   (system-get 'polynomial-xy)
   (system-get 'compression-lisp-math-function-xy)
   (system-get 'adf-lisp-math-function-xy)))

(defmethod drawablep ((o entity-function-x-y))
  "Answer whether <o> can be displayed on the GUI."
  t)

(defmethod pixel-step ((o entity-function-x-y))
  "Answers the quantity between each pixel through x or y axis."
  0.01)

(defmethod pixels-x ((o entity-function-x-y))
  "Answer default pixels over x axis for <o>."
  100)

(defmethod pixels-y ((o entity-function-x-y))
  "Answer default pixels over y axis for <o>."
  100)

(defmethod start-position-x ((o entity-function-x-y))
  "Answer the beggining of the image <o> on x axis."
  0)

(defmethod start-position-y ((o entity-function-x-y))
  "Answer the beggining of the image <o> on y axis."
  0)

(defmethod heigth ((o entity-function-x-y))
  "Answer <o> heigth."
  1)

(defmethod width ((o entity-function-x-y))
  "Answer <o> width."
  1)

(defmethod default-fitness-evaluators ((o entity-function-x-y))
  "Answer the default classes that can evaluate <o> fitness."
  (list 
   (system-get 'fine-0-10-function-x-y-evaluator)
   (system-get 'medium-0-10-function-x-y-evaluator)
   (system-get 'gross-0-10-function-x-y-evaluator)
   (system-get 'very-gross-0-10-function-x-y-evaluator)
   (system-get 'gross-pond-0-10-function-x-y-evaluator)
   (system-get 'sample-values-function-xy-evaluator)
   (system-get 'entity-seamless-basic)))

