(defparameter *infinite-productions-size-value* 1000000)



(defun structural-symbol (symbol)
  (or (eql symbol :open)
      (eql symbol :close)))

(defun test-minimum-production-size (all-productions production &optional passed)
  (if (and (symbolp production) (keywordp production))
      (if (structural-symbol production) 0 1)  
    (let ((q (non-recursive-right-productions-for all-productions (append (list production) passed)))
          (minimum-size *infinite-productions-size-value*))
      (dolist (i q)
        (let ((local-size 0))
          (dolist (j (cdr i))
            (incf local-size (test-minimum-production-size all-productions j (append (list production) passed))))
          (if (or (null minimum-size)
                  (< local-size minimum-size))
              (setf minimum-size local-size))))
      minimum-size)))

(defun test-calculate-minimum-production-size ()
  (let ((table (make-hash-table)))
    (dolist (p *test-productions*)
      (setf (gethash (car p) table)
            (test-minimum-production-size *test-productions* (car p))))
    table))

(defun non-recursive-right-productions-for (productions passed)
  (select
   ;; productions-for result
   productions 
   ;; non-recursive production condition
   (lambda (o)
     (and 
      (eql (first passed) (car o))
      (null (intersection passed (cdr o)))))))


;(updated-productions (grammar (system-get 'rgb-color-images)))

;(updated-productions (grammar (system-get 'rgb-color-images)))
;(updated-productions (grammar (test-language-glsl '(X Y))))
(setf *test-productions*
      (updated-productions (grammar (test-language-glsl '(X Y)))))

#|
'((START EXPRESION)
  (EXPRESION :OPEN 1-ARY-OPERATOR EXPRESION :CLOSE) 
  (EXPRESION :OPEN 2-ARY-OPERATOR EXPRESION EXPRESION :CLOSE)
  (EXPRESION :OPEN 3-ARY-OPERATOR EXPRESION EXPRESION EXPRESION :CLOSE)
  (EXPRESION CONSTANT)
  (EXPRESION VAR)
  (CONSTANT :CONSTANT)
  (VAR :VAR)
  (1-ARY-OPERATOR VEC-SIN) 
  (VEC-SIN :1-ARY-OPERATOR)
  (1-ARY-OPERATOR VEC-COS)
  (VEC-COS :1-ARY-OPERATOR)
  (2-ARY-OPERATOR VEC-+)
  (VEC-+ :2-ARY-OPERATOR)
  (2-ARY-OPERATOR VEC--)
  (VEC-- :2-ARY-OPERATOR)
  (2-ARY-OPERATOR VEC-*)
  (VEC-* :2-ARY-OPERATOR)
  (2-ARY-OPERATOR VEC-/-)
  (VEC-/- :2-ARY-OPERATOR)
  (2-ARY-OPERATOR COLOR-MAP-1)
  (COLOR-MAP-1 :2-ARY-OPERATOR)
  (2-ARY-OPERATOR VEC-INOISE-X-Y) 
  (VEC-INOISE-X-Y :2-ARY-OPERATOR)
  (2-ARY-OPERATOR VEC-PERLIN-X-Y)
  (VEC-PERLIN-X-Y :2-ARY-OPERATOR)
  (3-ARY-OPERATOR COLOR-MAP-3)
  (COLOR-MAP-3 :3-ARY-OPERATOR) 
  (3-ARY-OPERATOR 3-ARY-OPERATOR) 
  (2-ARY-OPERATOR 2-ARY-OPERATOR)
  (1-ARY-OPERATOR 1-ARY-OPERATOR)
  (3-ARY-OPERATOR 3-ARY-OPERATOR)
  (2-ARY-OPERATOR 2-ARY-OPERATOR)
  (1-ARY-OPERATOR 1-ARY-OPERATOR)))
|#