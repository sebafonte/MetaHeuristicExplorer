
(defclass pane-performance-test-runner-comparer (base-pane)
  ((test-containers-a :accessor test-containers-a)
   (test-containers-b :accessor test-containers-b)
   (comparison-result :accessor comparison-result)))


(defmethod match-and-compare ((p pane-performance-test-runner-comparer))
  nil)



