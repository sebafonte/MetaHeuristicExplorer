(proclaim '(optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))

;; Simplex Algorithm and its demonstration
;; Bruno Haible 25.09.1986, 30.5.1991, 22.4.1992
;; Common Lisp version
;; English version

;; Copyright (c) Bruno Haible 1991, 1992.
;; This file may be copied under the terms of the GNU General Public License.

(provide 'simplex)

; type of a number: here rational numbers
(deftype Zahl () 'rational) ; abbreviation: R
; Should this be changed to non-exact numbers (floats), uncomment the
; portion #| ... |# of code and eliminate the calls to the function rational.
(deftype Zahl-Vektor () '(simple-array Zahl 1)) ; abbreviation: R^n
(deftype Zahl-Matrix () '(simple-array Zahl 2)) ; abbreviation: R^mxn

; Notation:
; indices   1-based in mathematical theory, 0-based in this implementation
; A`        the transpose of A
; x[i]      (aref x i)
; A[i,j]    (aref A i j)
; <u,v>     u`*v = sum(i>=1, u[i]*v[i])

; Input: A, an mxn matrix of numbers,
;        b, an m vector of numbers,
;        c, an n vector of numbers.
; Solves a canonical optimisation problem:
; ( C ) Among the y in R^n with A*y=b, y>=0 find those with <c,y> = Min
; ( C* ) Among the z in R^m with x=A`*z+c>=0 find those with <b,z> = Min
; See: Koulikov, Alg`ebre et th'eorie des nombres, Chap. 6.1.
; Output: 1st value: flag if solvable, NIL or T,
;         if solvable:
;           2nd value: solution of ( C ), the linear problem LP:
;                      list containing
;                      - the solution y
;                      - the optimal value of <c,y>
;           3rd value: solution of ( C* ), the dual problem DP:
;                      list containing
;                      - the solution x
;                      - the constant terms Z for the z's
;                      - the dependencies ZZ between the z's
;                      - the optimal value of <b,z>
;                      The general solution has the following form:
;                      If ZZ[i,i]=T, the variable z_i is unrestricted.
;                      Otherwise ZZ[i,i]=NIL, and the variable z_i is calculated by
;                      z_i = Z[i] + sum(j=1,...,m with Z[j,j]=T , ZZ[i,j]*z_j) .
;           4th value: flag if (T) the given solutions make up the whole
;                      set of solutions or (NIL) there may be another solution.
;                      In either case the set of solutions is convex.
; Method:
; Method of Dantzig, see Koulikov book;
; enhancement for degenerate case following Bronstein/Semendjajew.
; [Thanks to all these russian mathematicians!]
(defun simplex (A b c)
  ; check input:
  (let ((m (length b))
        (n (length c)))
    (declare (type fixnum m n))
    (assert (typep A `(array * (,m ,n))))
    ; m = height of the matrix we are working in.
    ; n = width of the matrix we are working in.
    ; initialisation:
    (let ((AA (let ((h (make-array `(,m ,n))))
                (dotimes (i m)
                  (dotimes (j n)
                    (setf (aref h i j) (rational (aref A i j)))
                ) )
                h
          )   )
          (AB (let ((h (make-array m)))
                (dotimes (i m) (setf (aref h i) (rational (elt b i))))
                h
          )   )
          (AC (let ((h (make-array n)))
                (dotimes (j n) (setf (aref h j) (rational (elt c j))))
                h
          )   )
          (AD 0)
          ; AA in R^mxn : the tableau we work in
          ; AB in R^m : the right margin
          ; AC in R^n : the bottom margin
          ; AD in R : the bottom right corner
          (Extend nil)
          (AAE nil)
          ; Extend in Boolean : flag if the additional columns (degenerate case) are being used
          ; AAE in R^mxm : additional columns for the degenerate case
          (Zeile (let ((h (make-array m)))
                   (dotimes (i m) (setf (aref h i) (cons 'ZO i)))
                   h
          )      )
          (Spalte (let ((h (make-array n)))
                    (dotimes (j n) (setf (aref h j) (cons 'XY j)))
                    h
          )       )
          ; Zeile in Cons^m : left labels
          ; Spalte in Cons^n : top labels
          ;
          ; The tableau:
          ;
          ;             | Zeile[0]  Zeile[1]  ... Zeile[n-1]  |
          ; ------------+-------------------------------------+---------
          ; Spalte[0]   |  AA[0,0]   AA[0,1]  ... AA[0,n-1]   |  AB[0]
          ; Spalte[1]   |  AA[1,0]   AA[1,1]  ... AA[1,n-1]   |  AB[1]
          ;   ...       |    ...       ...          ...       |   ...
          ; Spalte[m-1] | AA[m-1,0] AA[m-1,1] ... AA[m-1,n-1] | AB[m-1]
          ; ------------+-------------------------------------+---------
          ;             |   AC[0]     AC[1]   ...   AC[n-1]   |   AD
          ;
          ; Labeling of columns:   0   or   Y
          ;                       ...      ...
          ;                        Z        X
          ;
          ; Labeling of rows:  Z ... 0  or  X ... -Y.
          ;
          ; for all i=0,...,m-1:
          ;   sum(j=0,...,n-1; AA[i,j]*(0 or Y[Spalte[j].Nr])) - AB[i] =
          ;     = (0 or -Y[Zeile[i].Nr])
          ; for all j=0,...,n-1:
          ;   sum(i=0,...,m-1; AA[i,j]*(Z[Zeile[i].Nr] or X[Zeile[i].Nr])) + AC[j] =
          ;     = (Z[Spalte[j].Nr] or X[Spalte[j].Nr])
          ; sum(j=1,...,N; AC[j]*(0 or Y[Spalte[j].Nr])) - AD = <c,y>
          ; sum(i=1,...,M; AB[i]*(Z[Zeile[i].Nr] or X[Zeile[i].Nr])) + AD = <b,z>
          ; These are to be considered as equations in the unknowns X,Y,Z.
          ;
          ; The additional columns - if present - are added at the right.
          ;
          (ZZ (let ((h (make-array `(,m ,m) :initial-element 0)))
                (dotimes (i m) (setf (aref h i i) nil))
                h
          )   )
          (Z (make-array m))
          (Y (make-array n))
          (X (make-array n))
         )
      (declare (type Zahl-Matrix AA) (type Zahl-Vektor AB AC) (type Zahl AD))
      (flet
        ((pivot (k l)
           (declare (type fixnum k l))
           ; pivots the tableau around the element AA[k,l] with 0<=k<m, 0<=l<n.
           ; The invariant is that before and after pivoting the above
           ; equations hold.
           (let ((r (/ (aref AA k l))))
             (declare (type Zahl r))
             ; column l :
             (progn
               (dotimes (i m)
                 (unless (eql i k)
                   (setf (aref AA i l) (- (* r (aref AA i l))))
             ) ) )
             (setf (aref AC l) (- (* r (aref AC l))))
             ; everything except row k and column l :
             (dotimes (j n)
               (unless (eql j l)
                 (let ((s (aref AA k j)))
                   (dotimes (i m)
                     (unless (eql i k)
                       (setf (aref AA i j) (+ (aref AA i j) (* s (aref AA i l))))
                   ) )
                   (setf (aref AC j) (+ (aref AC j) (* s (aref AC l))))
             ) ) )
             (let ((s (aref AB k)))
               (dotimes (i m)
                 (unless (eql i k)
                   (setf (aref AB i) (+ (aref AB i) (* s (aref AA i l))))
               ) )
               (setf AD (+ AD (* s (aref AC l))))
             )
             (when Extend
               (locally (declare (type Zahl-Matrix AAE))
                 (dotimes (j m)
                   (let ((s (aref AAE k j)))
                     (dotimes (i m)
                       (unless (eql i k)
                         (setf (aref AAE i j) (+ (aref AAE i j) (* s (aref AA i l))))
             ) ) ) ) ) )
             ; row k :
             (progn
               (dotimes (j n)
                 (unless (eql j l)
                   (setf (aref AA k j) (* (aref AA k j) r))
               ) )
               (setf (aref AB k) (* (aref AB k) r))
             )
             (when Extend
               (locally (declare (type Zahl-Matrix AAE))
                 (dotimes (j m)
                   (setf (aref AAE k j) (* (aref AAE k j) r))
             ) ) )
             ; element (k,l) :
             (setf (aref AA k l) r)
             ; swap labels:
             (rotatef (aref Zeile k) (aref Spalte l))
        )) )
        ; Bring the Z variables down (matrix may become smaller):
        (let ((elbar (make-array m :fill-pointer 0))
              (not-elbar (make-array m :fill-pointer 0)))
          ; elbar = set of the eliminatable z,
          ; not-elbar = set of the non-eliminatable z.
          (dotimes (i m)
            ; search maximum of absolute value in row i:
            (let ((hmax 0) (l nil))
              (dotimes (j n)
                (when (eq (car (aref Spalte j)) 'XY)
                  (let ((h (abs (aref AA i j))))
                    (when (> h hmax) (setq hmax h l j))
              ) ) )
              (if l
                ; AA[i,l] was the maximal element w.r.t. absolute value
                (progn
                  (vector-push i not-elbar)
                  (pivot i l)
                )
                ; trivial row
                (if (zerop (aref AB i))
                  ; Keep this dummy line; it will not change since we will
                  ; pivot only around XY columns and these columns have a 0
                  ; in row i.
                  ; Nonzero elements are only in ZO columns, and these
                  ; will not change.
                  (vector-push i elbar)
                  ; this row makes LP inconsistent -> unsolvable
                  (return-from simplex NIL)
          ) ) ) )
          ; Mark the eliminatable row in the ZZ matrix:
          ; The non-eliminatable rows have swapped positions with the X
          ; such that we have Spalte[(cdr Zeile[i])] = (ZO . i) if row i
          ; is non-eliminatable ( <==> (car Zeile[i]) = XY ).
          (dotimes (i0h (fill-pointer elbar))
            (let ((i0 (aref elbar i0h)))
              (setf (aref Z i0) 0) ; we must set Z[i0]=0 to make the other Z's correct!
              (setf (aref ZZ i0 i0) T) ; z_i0 is unrestricted
              (dotimes (ih (fill-pointer not-elbar))
                (let* ((i (aref not-elbar ih)) ; we must have (car Zeile[i])=XY
                       (j (cdr (aref Zeile i)))) ; column with which row i was pivoted
                  (setf (aref ZZ i i0) (aref AA i0 j))
          ) ) ) )
          ; delete rows: (uses that every not-elbar[i]>=i)
          (dotimes (ih (fill-pointer not-elbar))
            (let ((i (aref not-elbar ih)))
              (unless (eql ih i)
                (dotimes (j n) (setf (aref AA ih j) (aref AA i j)))
                (setf (aref AB ih) (aref AB i))
          ) ) )
          (setq m (fill-pointer not-elbar)) ; new number of rows = number of the ZO columns
        )
        ; sort columns: bring XY to the left, ZO to the right.
        ; This is used at the end to calculate the non-eliminatable z from the X.
        (let ((l 0) ; left column
              (r (1- n))) ; right column
          (loop
            (unless (< l r) (return))
            (cond ((eq (car (aref Spalte l)) 'XY) (incf l))
                  ((eq (car (aref Spalte r)) 'ZO) (decf r))
                  (t ; swap columns r and l
                     (dotimes (i m) (rotatef (aref AA i l) (aref AA i r)))
                     (rotatef (aref AC l) (aref AC r))
                     (rotatef (aref Spalte l) (aref Spalte r))
            )     )
        ) )
        ; hide these M columns from pivoting:
        (setq n (- n m))
        ; The elements AA[0..m-1,n..n+m-1], AC[n..n+m-1], Spalte[n,n+m-1]
        ; will only be used again at the end of phase 6.
        (let ((Zeile_save (copy-seq Zeile)))
          (flet
            ((SuchePivotZeile (AWZM l)
               ; For a choice set AWZM of rows and a column l choose the
               ; row k such that:
               ; We assume that
               ; for i in AWZM we have 0<=i<m and AB[i]>=0 und AA[i,l]>0.
               ; Among the i in AWZM, k is the one for which the quotient
               ; AB[i]/AA[i,l] is minimal. Should this quotient be =0, or
               ; if Extend=true, then afterwards Extend=true, and the vector
               ; (AB[i]/AA[i,l], AAE[i,1]/AA[i,l], ..., AAE[i,m]/AA[i,l])
               ; has been minimized (lexicographically) among all i in AWZM.
               ; If AWZM is empty, NIL is returned.
               (if (eql (fill-pointer AWZM) 0)
                 NIL
                 (let (k)
                   (unless Extend ; try to choose k
                     (let (hmax)
                       (dotimes (ih (fill-pointer AWZM))
                         (let* ((i (aref AWZM ih))
                                (h (/ (aref AB i) (aref AA i l))))
                           (when (or (eql ih 0) (< h hmax)) (setq hmax h k i))
                       ) )
                       (when (zerop hmax)
                         ; degenerate case
                         (setq Extend T)
                         (setq AAE (make-array `(,m ,m) :initial-element 0))
                         (dotimes (i m) (setf (aref AAE i i) 1))
                   ) ) )
                   (when Extend
                     ; The degenerate case has already been active or has
                     ; emerged now (then the old k may be bad).
                     (let (hmax hmaxe)
                       (dotimes (ih (fill-pointer AWZM))
                         (let ((i (aref AWZM ih)))
                           (let ((h (/ (aref AA i l)))
                                 (he (make-array m)))
                             (dotimes (j m) (setf (aref he j) (* (aref AAE i j) h)))
                             (setq h (* (aref AB i) h))
                             ; (h,he[1],...,he[M]) is the vector of quotients
                             (when (or (eql ih 0)
                                       ; instead of (< h hmax) now lexicographic comparison
                                       ; (h,he[1],...,he[M]) < (hmax,hmaxe[1],...,hmaxe[M]) :
                                       (or (< h hmax)
                                           (and (= h hmax)
                                                (dotimes (ie m NIL)
                                                  (let* ((he_ie (aref he ie))
                                                         (hmaxe_ie (aref hmaxe ie)))
                                                    (when (< he_ie hmaxe_ie) (return T))
                                                    (when (> he_ie hmaxe_ie) (return NIL))
                                   )   )   )    ) )
                               (setq hmax h hmaxe he k i)
                   ) ) ) ) ) )
                   k
            )) ) )
            (let ((PZM (make-array m :fill-pointer 0))
                  (p nil))
              ; PZM = set of the i with AB[i]>=0
              ; p = the last i for which AB[i] was being maximized
              (loop ; fill column AB with positive numbers
                #|
                ; throw away roundoff errors:
                (dotimes (ih (fill-pointer PZM))
                  (let ((i (aref PZM ih)))
                    (when (minusp (aref AB i)) (setf (aref AB i) 0))
                ) )
                |#
                (let ((NZM (make-array m :fill-pointer 0)))
                  ; NZM = set of the i with AB[i]<0
                  ; recalculate PZM and NZM:
                  (let ((old-PZM-count (fill-pointer PZM))) ; old cardinality of PZM
                    (setf (fill-pointer PZM) 0)
                    (dotimes (i m)
                      (if (>= (aref AB i) 0)
                        (vector-push i PZM)
                        (vector-push i NZM)
                    ) )
                    ; delete the additional columns if PZM really grew
                    ; and the degeneracy perhaps disappeared:
                    (when (> (fill-pointer PZM) old-PZM-count)
                      (setq Extend nil)
                      (setq p nil)
                    )
                    ; otherwise PZM remained unchanged, and AB[p]<0 still holds.
                  )
                  (when (eql (fill-pointer NZM) 0) (return)) ; every AB[i]>=0 ?
                  (if p
                    ; use the last p.
                    (when (dotimes (j n t) (when (< (aref AA p j) 0) (return nil)))
                      ; every AA[p,j]>=0 but AB[p]<0
                      ; ==> row makes LP inconsistent ==> unsolvable
                      (return-from simplex NIL)
                    )
                    ; choose p: p := the i among those with AB[i]<0
                    ; for which the number of AA[i,j]>=0 is maximal.
                    (let ((countmax -1))
                      (dotimes (ih (fill-pointer NZM))
                        (let ((i (aref NZM ih))
                              (count 0))
                          (dotimes (j n) (when (>= (aref AA i j) 0) (incf count)))
                          (when (> count countmax) (setq countmax count p i))
                      ) )
                      (when (eql countmax n)
                        ; every AA[p,j]>=0 but AB[p]<0
                        ; ==> row makes LP inconsistent ==> unsolvable
                        (return-from simplex NIL)
                    ) )
                  )
                  ; Now AB[p]<0, and there is a j with AA[p,j]<0.
                  ; Choose l: maximal abs(AA[p,j]) among the j with AA[p,j]<0.
                  (let ((hmin 0) (l nil))
                    (dotimes (j n)
                      (let ((h (aref AA p j)))
                        (when (< h hmin) (setq hmin h l j))
                    ) )
                    ; build AWZM:
                    (let ((AWZM (make-array m :fill-pointer 0)))
                      (dotimes (ih (fill-pointer PZM))
                        (let ((i (aref PZM ih)))
                          (when (> (aref AA i l) 0) (vector-push i AWZM))
                      ) )
                      (let ((k (SuchePivotZeile AWZM l)))
                        (if (null k)
                          ; Pivoting around AA[p,l] lets PZM grow at least
                          ; by the element p because:
                          ; for i in PZM we have AB[i]>=0 and
                          ; (because AZWM={}) also AA[i,l]<=0,
                          ; and then afterwards
                          ; AB[i]=AB[i]-AB[p]*AA[i,l]/AA[p,l] >= 0,
                          ;        >=0   <0     <=0     <0
                          ; that is i in PZM again.
                          ; But afterwards we also have
                          ; AB[p]=AB[p]/AA[p,l] (<0/<0) >0,
                          ; therefore p in PZM.
                          (progn
                            (setq Extend nil) ; additional columns aren't needed any more
                            (pivot p l)
                          )
                          ; pivot row k chosen, ready for pivoting.
                          ; Pivoting around AA[k,l] does not shrink PZM
                          ; because:
                          ; for i in PZM we have AB[i]>=0.
                          ; If AA[i,l]<=0, we have afterwards
                          ; AB[i]=AB[i]-AB[k]*AA[i,l]/AA[k,l] >=0.
                          ;        >=0   >=0    <=0     >0
                          ; But if AA[i,l]>0, then for every i/=k afterwards
                          ; AB[i]=AA[i,l]*(AB[i]/AA[i,l]-AB[k]/AA[k,l])     >=0
                          ;          >0    >=0 because of the choice of k and i in AWZM
                          ; and for i=k afterwards AB[k]=AB[k]/AA[k,l] (>=0/>0) >=0.
                          ; Therefore afterwards always AB[i]>=0, i.e. i in PZM.
                          (pivot k l)
                ) ) ) ) )
            ) )
            ; Now Extend=false, since (fill-pointer PZM) must just have
            ; grown, reaching m.
            ; From now on every AB[i]>=0, and this property remains.
            (loop
              ; search an l with AC[l]<0 :
              (let (l)
                (when
                  (dotimes (j n T)
                    (when (< (aref AC j) 0) (setq l j) (return NIL))
                  )
                  (return) ; every AC[j]>=0 ==> solvable
                )
                ; AWZM := set of the i with AA[i,l]>0 :
                (let ((AWZM (make-array m :fill-pointer 0)))
                  (dotimes (i m)
                    (when (> (aref AA i l) 0) (vector-push i AWZM))
                  )
                  (let ((k (SuchePivotZeile AWZM l)))
                    (if (null k)
                      ; every AA[i,l]<=0 and AC[l]<0 ==> column makes DP inconsistent
                      (return-from simplex NIL)
                      (pivot k l)
                      ; still every AB[i]>=0.
                      ; AD:=AD-AB[k]*AC[l]/AA[k,l] >= AD is not lowered.
                      ;         >=0   <0     >0
              ) ) ) )
            )
            ; Solvable! Build solution:
            (let ((complete t))
              (dotimes (i m)
                (let ((s (aref AB i))
                      (index (cdr (aref Zeile i))))
                  (setf (aref X index) 0 (aref Y index) s)
                  (setq complete (and complete (> s 0)))
              ) )
              (dotimes (j n)
                (let ((s (aref AC j))
                      (index (cdr (aref Spalte j))))
                  (setf (aref X index) s (aref Y index) 0)
                  (setq complete (and complete (> s 0)))
              ) )
              ; The non-eliminatable z values are calculated from the hidden
              ; parts of AA and AC and the values of X and Y :
              (do ((j n (1+ j)))
                  ((>= j (+ n m)))
                (let ((s (aref AC j)))
                  (dotimes (i m)
                    (setq s (+ s (* (aref AA i j) (aref X (cdr (aref Zeile_save i))))))
                  )
                  (setf (aref Z (cdr (aref Spalte j))) s)
              ) )
              (values T (list Y (- AD)) (list X Z ZZ AD) complete)
) ) ) ) ) ) )

(defun test-simplex (aufg)
  (multiple-value-bind (m n) (values-list (array-dimensions (first aufg)))
    (flet ((list-1-to (n) ; list of the numbers from 1 to n
             (do* ((l '() (cons i l))
                   (i n (1- i)))
                  ((eql i 0) l)
          )) )
      (let ((x-list
              (mapcar #'(lambda (i) (format nil "X[~D]" i)) (list-1-to n))
            )
            (y-list
              (mapcar #'(lambda (i) (format nil "Y[~D]" i)) (list-1-to n))
            )
            (z-list
              (mapcar #'(lambda (i) (format nil "Z[~D]" i)) (list-1-to m))
           ))
        ; output problem:
        (multiple-value-bind (A b c) (values-list aufg)
          (format t "~%~%Linear problem:")
          (dotimes (i m)
            (let ((zeile (make-array n)))
              (dotimes (j n) (setf (aref zeile j) (aref A i j)))
              (format t "~%~{~1{~S * ~A~:}~^ + ~} = ~S"
                        (remove 0 (mapcar #'list (coerce zeile 'list) y-list)
                                  :key #'first :test #'=
                        )
                        (aref b i)
          ) ) )
          (format t "~%with ~{~A~^, ~} >= 0" y-list)
          (format t "~%Minimize ~{~1{~S * ~A~:}~^ + ~}."
                    (remove 0 (mapcar #'list (coerce c 'list) y-list)
                              :key #'first :test #'=
          )         )
          (format t "~%Dual problem:")
          (dotimes (j n)
            (let ((spalte (make-array m)))
              (dotimes (i m) (setf (aref spalte i) (aref A i j)))
              (format t "~%~A = ~:{~S * ~A + ~} ~S >= 0"
                        (elt x-list j)
                        (remove 0 (mapcar #'list (coerce spalte 'list) z-list)
                                  :key #'first :test #'=
                        )
                        (aref c j)
          ) ) )
          (format t "~%Minimize ~{~1{~S * ~A~:}~^ + ~}."
                    (remove 0 (mapcar #'list (coerce b 'list) z-list)
                              :key #'first :test #'=
          )         )
        )
        (let ((solution (multiple-value-list (apply #'simplex aufg))))
          ; output solution:
          (if (first solution)
            (progn
              (format t "~%~%~:[One~;The only~] solution is:" (fourth solution))
              (let ((LP-solution (second solution)))
                (format t "~%~{~1{~A = ~S~:}~^, ~}~%Minimum = ~S"
                          (mapcar #'list y-list (coerce (first LP-solution) 'list))
                          (second LP-solution)
              ) )
              (let ((DP-solution (third solution)))
                (let ((Z (second DP-solution))
                      (ZZ (third DP-solution)))
                  (dotimes (i m)
                    (format t "~%~A = " (elt z-list i))
                    (if (aref ZZ i i)
                      (format t "arbitrary")
                      (format t "~S~:{ + ~S * ~A~}"
                                (aref Z i)
                                (let ((L nil))
                                  (dotimes (j m)
                                    (when (aref ZZ j j)
                                      (unless (zerop (aref ZZ i j))
                                        (push (list (aref ZZ i j) (elt z-list j))
                                              L
                                  ) ) ) )
                                  (nreverse L)
                      )         )
                ) ) )
                (format t "~%~{~1{~A = ~S~:}~^, ~}~%Minimum = ~S"
                          (mapcar #'list x-list (coerce (first DP-solution) 'list))
                          (fourth DP-solution)
            ) ) )
            (format t "~%unsolvable")
          )
          solution
) ) ) ) )

(defun test ()
  (mapcar #'test-simplex
    '((#2A((1 0 1 -1  -1  3)
           (0 1 2 -1 -1/2 1))
       #(0 0)
       #(0 0 -1 -1 -3 8)
      )
      (#2A((1 -1  -1  3 1 0)
           (2 -1 -1/2 1 0 1))
       #(0 0)
       #(-1 -1 -3 8 0 0)
      )
      (#2A((1 1 0 1)
           (2 0 1 0))
       #(1 1)
       #(-1 1 -2 0)
      )
      (#2A((1  1 2 1 3 -1  0)
           (2 -2 3 1 1  0 -1))
       #(4 3)
       #(2 3 5 2 3 0 0)
      )
      (#2A(( 1  8  0  -1  0  0  0)
           (-3 -7 -20  0 -1  0  0)
           ( 1  0  0   0  0 -1  0)
           ( 0 -1  0   0  0  0 -1)
           ( 1  1  1   0  0  0  0))
       #(3 -6 2/5 -1/2 1)
       #(5 2 1/4 0 0 0 0)
      )
      (#2A((1 2  1  1 -2)
           (1 1 -4 -2 -3)
           (1 2  5 -4  6))
       #(4 2 3)
       #(3 5 4 5 6)
      )
      (#2A((1  2 -1  1)
           (2 -2  3  3)
           (1 -1  2 -1))
       #(0 9 6)
       #(-3 1 3 -1)
      )
      (#2A((-1  0  0  0  0 -1 1 0 0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0)
           ( 0 -1  0  0  0  0 0 1 0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0)
           (-1  1  0 -1 -1  1 0 0 1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0)
           ( 1  0  0  1  0  0 0 0 0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0)
           ( 1 -1  0  0  0  0 0 0 0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0)
           ( 0  1 -2  0  0  0 0 0 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0)
           ( 0  0  1 -1  0  0 0 0 0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0)
           ( 0  0  1  0 -1  0 0 0 0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0)
           ( 0  0  0  1  0 -1 0 0 0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0)
           ( 0  0  0  0  1  0 0 0 0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0)
           ( 0  0  0  0  1  0 0 0 0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0)
           ( 0  0  0  0  0  1 0 0 0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0)
           ( 0  0  0  0  0  0 0 0 0  0  0  0  1  0  0  0  0  0  0  0  0 -1  0)
           ( 0  0  0  0  0  0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  1  0 -1)
          )
       #(0 0 0 0 0 0 0 0 0 0 0 0 1 1)
       #(0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
     ))
) )

