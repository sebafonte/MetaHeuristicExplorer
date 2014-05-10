
(defclass one-point-array-crossover (binary-genetic-operator)
  ())

(defmethod operate ((o one-point-array-crossover) language exps)
  (let* ((point-bit (random-integer 0 (bits language)))
         (bits (bits language))
         (a (first exps))
         (b (second exps))
         (value-a (make-array bits :element-type 'bit))
         (value-b (make-array bits :element-type 'bit)))
    (dotimes (i bits)
      (setf (aref value-a i) (if (< i point-bit) (aref b i) (aref a i))
            (aref value-b i) (if (< i point-bit) (aref a i) (aref b i))))
    (values value-a value-b)))


(defclass one-point-array-mutation (unary-genetic-operator)
  ())

(defmethod operate ((o one-point-array-mutation) language exps)
  (let ((bit-value (random-integer 0 (bits language)))
        (value (copy (first exps))))
    (setf (aref value bit-value)
          (if (= (aref value bit-value) 1) 0 1))
    value))


#|
;; #TEST
(setf language (make-instance 'binary-language :bits 5))
(setf parents (list (create-new-random-valid language nil)))
(print parents)
(operate (make-instance 'one-point-array-mutation) language (list #*00000))
(operate (make-instance 'one-point-array-mutation) language (list #*00000))
(operate (make-instance 'one-point-array-mutation) language (list #*00000))


(setf language (make-instance 'binary-language :bits 5))
(setf parents (list (create-new-random-valid (make-instance 'binary-language :bits 5) nil)
                    (create-new-random-valid (make-instance 'binary-language :bits 5) nil)))
(print parents)
(operate (make-instance 'one-point-array-crossover) language (list #*00000 #*11111))
(operate (make-instance 'one-point-array-crossover) language (list #*00000 #*11111))
(operate (make-instance 'one-point-array-crossover) language (list #*00000 #*11111))
|#