
(defclass one-point-array-crossover (binary-genetic-operator)
  ())

(defmethod operate ((o one-point-array-crossover) language exps)
  (let* ((point (random-integer 0 (elements language)))
         (elements (elements language))
         (a (first exps))
         (b (second exps))
         (value-a (make-array elements :element-type (array-element-type a)))
         (value-b (make-array elements :element-type (array-element-type b))))
    (dotimes (i elements)
      (setf (aref value-a i) (if (< i point) (aref b i) (aref a i))
            (aref value-b i) (if (< i point) (aref a i) (aref b i))))
    (values value-a value-b)))


(defclass one-point-array-mutation (unary-genetic-operator)
  ())

(defmethod operate ((o one-point-array-mutation) language exps)
  (let ((bit-value (random-integer 0 (elements language)))
        (value (copy (first exps))))
    (setf (aref value bit-value)
          (mutate-value language (aref value bit-value)))
    value))


#|
;; #TEST
(setf language (make-instance 'binary-language :elements 5))
(setf parents (list (create-new-random-valid language nil)))
(print parents)
(operate (make-instance 'one-point-array-mutation) language (list #*00000))
(operate (make-instance 'one-point-array-mutation) language (list #*00000))
(operate (make-instance 'one-point-array-mutation) language (list #*00000))


(setf language (make-instance 'binary-language :elements 5))
(setf parents (list (create-new-random-valid (make-instance 'binary-language :elements 5) nil)
                    (create-new-random-valid (make-instance 'binary-language :elements 5) nil)))
(print parents)
(operate (make-instance 'one-point-array-crossover) language (list #*00000 #*11111))
(operate (make-instance 'one-point-array-crossover) language (list #*00000 #*11111))
(operate (make-instance 'one-point-array-crossover) language (list #*00000 #*11111))
|#