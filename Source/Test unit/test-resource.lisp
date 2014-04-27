
(defclass test-resource (base-model)
  ())


(defmethod available ((o test-resource))
  nil)

(defmethod set-up ((o test-resource))
  nil)

(defmethod tear-down ((o test-resource))
  nil)

(defmethod reset ((o test-resource))
  "Reset <o> state."
  nil)