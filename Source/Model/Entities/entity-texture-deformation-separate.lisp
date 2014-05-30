
(defclass entity-texture-deformation-separate (entity-texture-deformation)
  ())


(defparameter *texture-deformation-separate-expression-tokens*
  '(;; Separator
    (values :values-list)
    ;; 1 argument operators
    (abs :1-ary-operator)
    (sin :1-ary-operator)
    (cos :1-ary-operator)
    (tan :1-ary-operator)
    (plog :1-ary-operator)
    ;; 2 argument operators
    (+ :2-ary-operator)
    (- :2-ary-operator)
    (* :2-ary-operator)
    (/- :2-ary-operator)
    (perlin-x-y :2-ary-operator)))


(defmethod possible-languages ((o entity-texture-deformation-separate))
  (list 
   (system-get 'rgb-color-images-separate)))

(defun texture-deformation-separate-expression-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol (texture-deformation-separate-expression-get-token grammar symbol)
      nil)))

(defun texture-deformation-separate-expression-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (when (equal token-type :unknown)
        (if (or (numberp word) 
                (equal (class-name (class-of word)) 'image-vector-3d))
            (setf token-type :constant)))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-texture-deformation-separate-expression-parser (name)
  (eval
   `(defparser ,name
               ((start values-expression)
                $1)
               ((values-expression :open :values-list expresion expresion :close)
                `(:values-list ((values ,$2) ,$3 ,$4)))
               ((expresion :open :1-ary-operator expresion :close)
                `(:expresion (,$2 ,$3)))
               ((expresion :open :2-ary-operator expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4)))
               ((expresion :constant)
                `(:expresion ,$1))
               ((expresion :var)
                `(:expresion ,$1)))))

(defun texture-deformation-separate-expression-grammar-productions ()
  '((start values-expression)
    (values-expression :open :values-list expresion expresion :close)
    (expresion :open 1-ary-operator expresion :close)
    (expresion :open 2-ary-operator expresion expresion :close)
    (expresion constant)
    (expresion var)
    (constant :constant)
    (var :var)))

(defmethod simplify-texture-deformation-separate (expression strategy language)
  (list 'values 
        (simplify-strategy (cadr expression) strategy language)
        (simplify-strategy (caddr expression) strategy language)))

;; Methods for fitness evaluators
(defmethod point-difference ((o entity-texture-deformation-separate) function a b c d)
  "Answer difference value for a scalar <function> object over points <a,b> and <c,d>."  
  (point-difference-separate function a b c d))

(defun point-difference-separate (function a b c d)
  "Answer difference value for a scalar <function> object over points <a,b> and <c,d>."
  (multiple-value-bind (a-x a-y)
      (point-value function a b)
    (multiple-value-bind (b-x b-y)
        (point-value function c d)
      (let ((dx (- b-x a-x)) 
            (dy (- b-y a-y)))
        (+ (abs dx) (abs dy))))))

(defmethod constant-p ((o entity-texture-deformation-separate) 
                       &optional (check-genotype t) (check-phenotype t))
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
               (first-value-a)
               (first-value-b)
               (x)
               (y))
          (declare (special x) (special y))
          (block 1
            (dotimes (i pixels-x)
              (dotimes (j pixels-y)
                (setf x (+ start-x (* i delta-x))
                      y (+ start-y (* j delta-y)))
                (multiple-value-bind (value-a value-b)
                    (funcall function)
                  (if first-value-a
                      (if (not (or (= value-a first-value-a)
                                   (= value-b first-value-b)))
                          (return-from 1 nil))
                    (setf first-value-a value-a
                          first-value-b value-b)))))
            t))
      nil)))