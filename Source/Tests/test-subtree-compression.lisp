(defclass test-compression-subroutine-manager (test-base-model)
  ((manager :initarg :manager :accessor manager)))


#|
(defmethod test-reset ((o test-compression-subroutine-manager))
  nil)

(defmethod test-internalize-function ((o test-compression-subroutine-manager))
  nil)

(defmethod test-internalize-function-with-arguments ((o test-compression-subroutine-manager))
  nil)

(defmethod test-subroutines-count ((o test-compression-subroutine-manager))
  nil)

(defmethod test-subtree-for ((o test-compression-subroutine-manager))
  nil)

(defmethod test-has-subroutine ((o test-compression-subroutine-manager))
  nil)
|#


(defclass test-subroutine-compression-operator (test-base-model)
  ())

(defmethod test-leaf-compression-1 ((o test-subroutine-compression-operator))
  "Execute leaf compression operation several times checking for parsing errors."
  (let ((test-language (system-get-copy 'compression-lisp-math-function-xy)))
    (dotimes (i 20)
      (parse
       (grammar test-language)
       (compress-1
        '(* (sin (cos (sin x))) (sqr (abs (sqr y))))
        test-language
        (system-get 'compress-1))))))

(defmethod test-depth-compression-1 ((o test-subroutine-compression-operator))
  "Execute depth compression operation several times checking for parsing errors."
  (let ((test-language (system-get-copy 'compression-lisp-math-function-xy)))
    (dotimes (i 20)
      (parse
       (grammar test-language)
       (compress-2
        '(* (sin (cos (sin x))) (sqr (abs (sqr y))))
        test-language
        (system-get 'compress-2))))))

#|
(defmethod test-expand-1 ((o test-subroutine-compression-operator))
  "Execute expand operation several times checking for parsing errors."
  (let* ((operator (system-get 'compress-2))
         (language (system-get-copy 'compression-lisp-math-function-xy))
         (result (apply (value-function operator) (list (program o) language operator))))
    ()))

(defmethod test-expand-all-1 ((o test-subroutine-compression-operator))
  "Execute expand all operation several times checking for parsing errors and subroutine calls."
  nil)
|#