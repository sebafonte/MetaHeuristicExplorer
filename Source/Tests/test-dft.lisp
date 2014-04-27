(defclass test-dft (test-base-model) ())


(defmethod check-complex-array-value-similar ((o test-dft) a b)
  "Answer whether <a> and <b> arrays of complex are similar for <o>."
  (and (= (length a) (length b))
       (< (compare-seqs-for-test a b) 0.001)))

(defun compare-seqs-for-test (a b)
  (let ((error 0))
    (dotimes (i (length a))
      (incf error (abs (- (aref a i) (aref b i)))))
    (values error)))

(defmethod test-dft-and-inverse-dft-case-1 ((o test-dft))
 (check (check-complex-array-value-similar 
         o
         (inverse-dft! (dft! (vector 1.0 6.2 pi -7.0)))
         (vector 1.0 6.2 pi -7.0))))

(defmethod test-dft-and-inverse-dft-case-2 ((o test-dft))
   (check (check-complex-array-value-similar 
           o
           (inverse-dft! (dft! (vector 1.0 6.2 pi -7.0 (- pi))))
           (vector 1.0 6.2 pi -7.0 (- pi)))))

(defmethod test-dft-and-inverse-dft-case-3 ((o test-dft))
  ;;; FFT test from Singleton's IEEE article ...
  (let ((c-v (vector #C(0.22925607 0.76687502)
                     #C(0.68317685 0.50919111)
                     #C(0.87455959 0.64464100)
                     #C(0.84746840 0.35396343)
                     #C(0.39889159 0.45709421)
                     #C(0.23630936 0.13318189)
                     #C(0.16605222 0.22602680)
                     #C(0.66245903 0.25021174)
                     #C(0.61769668 0.26246527)
                     #C(0.51266762 0.93920734)
                     #C(0.62402861 0.42238195)
                     #C(0.93970599 0.28206823)
                     #C(0.46921754 0.054879178)
                     #C(0.51983086 0.39682690)
                     #C(0.11315656 0.60751725)
                     #C(0.70150672 0.88705479))))
    (check (check-complex-array-value-similar 
            o
            (inverse-fft! (fft! (copy-seq c-v)))
            c-v))))

(defmethod test-dft-kay-case-1 ((o test-dft))
  ;;; Test case from Kay ...
  (check (check-complex-array-value-similar 
          o
          (fft-kay! #(1.0 #C(0.0 1.0) -1.0 #C(0.0 -1.0) 1.0 #C(0.0 1.0) -1.0 #C(0.0 -1.0)))
          #(#c(0.0 0.0) 
            #c(0.0 0.0) 
            #c(8.0 2.4492127076447545E-16) 
            #c(0.0 0.0) 
            #c(0.0 0.0) 
            #c(0.0 0.0) 
            #c(0.0 -2.4492127076447545E-16) 
            #c(0.0 0.0)))))

(defmethod test-dft-and-inverse-dft-kay-case-1 ((o test-dft))
  (let ((vector #(1.0 #C(0.0 2.0) -3.0 #C(0.0 -4.0) 5.0 #C(0.0 6.0) -7.0 #C(0.0 -8.0))))
    (check (check-complex-array-value-similar 
            o
            (inverse-fft-kay! (fft-kay! (copy-seq vector)))
            vector))))
