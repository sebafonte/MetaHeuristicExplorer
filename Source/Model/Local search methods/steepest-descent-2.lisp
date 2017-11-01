;;; Steepest descent method copyed from "Analisis Numérico 6th edition, Richard L. Burden / J. Douglas Faires"
;;; Not working properly i think (it has some error).
;;;

(defmethod constant-optimization-steepest-descent ((source object-in-search) (s optimization-strategy))
  "Optimize <object> constants using <s> steepest descent like optimization."
  (let* ((o (object source))
         (values (get-constants-information o))
         (gradient (make-array (length values)))
         (precision (precision s))
         (max-iterations (max-iterations s))
         (algorithm (algorithm (context source)))
         (delta-gradient 0.01)
         (a)
         (b)
         (g)
         (g0)
         (g1)
         (g2)
         (g3)
         (o0)
         (o1)
         (o2)
         (o3))
    ;; #TODO: Check if it`s necessary to evaluate object here, it should'nt be necessary
    (evaluate algorithm o)
    (setf g (fitness o))
    (let ((result (block block-main
      ;; STEP 1 y 2: While delta-fitness is'nt very small
      (do ((i 0 (1+ i)))
          ((> i max-iterations) 
           nil)
        ;; STEP 3: Save g1
        (setf g1 (fitness o))
        (setf o1 (copy o))
        ;; STEP 3: Calculate gradient
        (calculate-gradient o delta-gradient algorithm values gradient)
        ;; STEP 4: If gradient = 0, stop
        (if (= 0 (reduce '+ (map 'vector (lambda (x) (* x x)) gradient)))
            (return-from block-main o))
        ;; STEP 5: Normalize gradient
        (setf gradient (normalize-gradient gradient))
        ;; STEP 5: Initialize alpha
        (let ((alpha-1 0)
              (alpha-3 1))
          ;; STEP 5: Determine g3
          (evaluate-with-costants o algorithm alpha-3 gradient values)
          (setf o3 (copy o))
          (setf g3 (fitness o3))
          ;; STEP 6: While g3 >= g1 do 7 and 8
          (do ((j 1 (1+ j)))
              ((< g3 g1) 
               o)
            ;; STEP 7: Divide alpha-3
            (setf alpha-3 (/ alpha-3 2))
            ;; STEP 7:  Move to the point and evaluate
            (evaluate-with-costants o algorithm alpha-3 gradient values)
            (setf o3 (copy o))
            (setf g3 (fitness o3))
            ;; STEP 8: If alpha-3 > TOL/2
            (if (< alpha-3 (/ precision 2))
                (return-from block-main o1)))
          ;; STEP 9: 
          (let ((alpha-2 (/ alpha-3 2)))
            (evaluate-with-costants o algorithm alpha-2 gradient values)
            (setf o2 (copy o))
            (setf g2 (fitness o2))
            ;; STEP 10: 
            (let* ((h1 (/ (- g2 g1) alpha-2))
                   (h2 (/ (- g3 g2) (- alpha-3 alpha-2)))
                   (h3 (/ (- h2 h1) alpha-3)))
              ;; STEP 11:
              (let ((alpha-0 (* 0.5 (- alpha-2 (/ h1 h3)))))
                (evaluate-with-costants o algorithm alpha-0 gradient values)
                (setf o0 (copy o))
                (setf g0 (fitness o2))
                ;; STEP 12: evaluate with alpha-1
                (evaluate-with-costants o algorithm alpha-1 gradient values)
                (setf o1 (copy o))
                (setf g1 (fitness o))
                ;; STEP 12: evaluate with alpha-2
                (evaluate-with-costants o algorithm alpha-3 gradient values)
                (setf o3 (copy o))
                (setf g3 (fitness o))
                (setf alpha (if (< g0 g3) alpha-0 alpha-3))
                ;; STEP 13:
                (evaluate-with-costants o algorithm alpha gradient values)
                (setf g (fitness o))
                ;; STEP 14: 
                (if (< (abs (- g g1)) precision)
                    (return-from block-main o))))))))))
    (let ((value (copy source)))
      (setf (object value) result)
      value))))
