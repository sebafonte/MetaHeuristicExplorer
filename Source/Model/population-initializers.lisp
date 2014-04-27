(defclass population-population-initializer (base-model)
  ((name :initarg :name :initform "Population initializer" :accessor name)))


(defclass random-population-initializer (population-population-initializer)
  ())

(defmethod initialize-population ((initializer random-population-initializer) (p population))
  "Initialize population with initializer."
  nil)


(defclass ramped-half-and-half-population-initialized (population-population-initializer)
  ((min-value :initarg :min-value :initform nil :accessor min-value)
   (max-value :initarg :max-value :initform nil :accessor max-value)))

(defmethod initialize-population ((initializer ramped-half-and-half-population-initialized) (p population))
  "Initialize population with initializer."
  nil)


(defclass static-population-initializer (population-population-initializer)
  ((initial-individuals :initarg :initial-individuals :initform nil :accessor initial-individuals)))

(defmethod initialize-population ((initializer static-population-initializer) (p population))
  "Initialize population with initializer."
  nil)


