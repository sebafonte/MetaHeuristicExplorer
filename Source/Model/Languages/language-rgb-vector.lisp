
(defparameter *lisp-rgb-vector-tokens*
  '(;; 1 argument operators
    (vecabs :1-ary-operator)
    (vecsqr :1-ary-operator)
    (vecsin :1-ary-operator)
    (veccos :1-ary-operator)
    (vectan :1-ary-operator)
    ;; 2 argument operators
    (vecadd :2-ary-operator)
    (vecsubstract :2-ary-operator)
    (vecmultiply :2-ary-operator)
    (vecdiv :2-ary-operator)
    ;; 3 argument operators
    (veccolormap :3-ary-operator)
    (createvector :create-vector)))

;; #NOTE: Extension for supporting three textures and three sub images. 
;;        Only for client evaluation, #'vecimg? is not implemented in the Lisp image yet.
(defparameter *lisp-rgb-vector-tokens-extended*
  (append *lisp-rgb-vector-tokens*
          '((vecfa :2-ary-operator) (vecfb :2-ary-operator) (vecfc :2-ary-operator)
            (vectexa :2-ary-operator) (vectexb :2-ary-operator) (vectexc :2-ary-operator))))


(defun rgb-vector-expression-lexer (grammar)
  (declare (special *parser-input*))
  (let ((symbol (pop *parser-input*)))
    (if symbol (rgb-vector-expression-get-token grammar symbol)
      nil)))

(defun rgb-vector-expression-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (when (equal token-type :unknown)
        (if (numberp word) 
            (setf token-type :constant))
        (setf token-type :constant))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-rgb-vector-expression-parser (name)
  (eval
   `(defparser ,name
               ((start exp)
                $1)
               ((exp :open :1-ary-operator exp :close)
                `(:exp (,$2 ,$3)))
               ((exp :open :2-ary-operator exp exp :close)
                `(:exp (,$2 ,$3 ,$4)))
               ((exp :open :3-ary-operator exp exp exp :close)
                `(:exp (,$2 ,$3 ,$4 ,$5)))
               ((exp vector)
                `(:exp ,$1))
               ((vector :open :create-vector scalar scalar scalar :close)
                `(:vector (,$2 ,$3 ,$4 ,$5)))
               ((scalar constant)
                `(:scalar ,$1))
               ((scalar var)
                `(:scalar ,$1))
               ((constant :constant)
                `(:constant ,$1))
               ((var :var)
                `(:var ,$1)))))

(defun rgb-vector-expression-grammar-productions ()
  '((start exp)
    (exp :open 1-ary-operator exp :close)
    (exp :open 2-ary-operator exp exp :close)
    (exp :open 3-ary-operator exp exp exp :close)
    (exp vector)
    (vector :open :create-vector scalar scalar scalar :close)
    (scalar constant)
    (scalar var)
    (constant :constant)
    (var :var)))

#|
 (let* ((language (copy-cyclic (system-get 'rgb-color-images-vector)))
        (max-size 30))
    (setf (max-size language) max-size)
    (let ((result (create-random-from-production language '(start) max-size nil)))
      (format nil "~A | ~A" result (infix-coverted-string result))))
|#

(defun vecabs (x)
  (to-array (list (abs (aref x 0)) (abs (aref x 1)) (abs (aref x 2)))))

(defun vecsqr (x)
  (to-array (list (sqr (aref x 0)) (sqr (aref x 1)) (sqr (aref x 2)))))

(defun vecsin (x)
  (to-array (list (sin (aref x 0)) (sin (aref x 1)) (sin (aref x 2)))))

(defun veccos (x)
  (to-array (list (cos (aref x 0)) (cos (aref x 1)) (cos (aref x 2)))))

(defun vectan (x)
  (to-array (list (tan (aref x 0)) (tan (aref x 1)) (tan (aref x 2)))))

(defun vecadd (x y)
  (to-array (list (+ (aref x 0) (aref y 0)) (+ (aref x 1) (aref y 1)) (+ (aref x 2) (aref y 2)))))
	
(defun vecsubstract (x y)
  (to-array (list (- (aref x 0) (aref y 0)) (- (aref x 1) (aref y 1)) (- (aref x 2) (aref y 2)))))
	
(defun vecmultiply (x y)
  (to-array (list (* (aref x 0) (aref y 0)) (* (aref x 1) (aref y 1)) (* (aref x 2) (aref y 2)))))
	
(defun vecdiv (x y)
  (to-array (list (/- (aref x 0) (aref y 0)) (/- (aref x 1) (aref y 1)) (/- (aref x 2) (aref y 2)))))
	
(defun veccolormap (x y z)
  (to-array (list (/- (aref x 0) 10.0) (/- (aref y 0) 10.0) (/- (aref z 0) 10.0))))
	
(defun createvector (x y z)
  (to-array (list x y z)))

