
;; Special differential evolution operators
(defclass de-crossover (binary-genetic-operator)
  ((parameter :initarg :parameter :accessor parameter)))

(defclass de-mutation (unary-genetic-operator)
  ((scaling :initarg :scaling :accessor scaling)))


;; Random initializer: random creation population could delegate here
(defclass de-random-initializer ()
  ())

(defmethod generate-individual ()
  nil)

(defmethod generate-population ()
  nil)




