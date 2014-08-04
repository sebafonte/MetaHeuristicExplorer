(defclass test-utilities (test-base-model)
  ())


(defmethod test-maximum-of-list ((o test-utilities))
  (check 
    (= (maximum-of '(1 2 3) (lambda (value) (+ 1 value))) 3)
    (= (maximum-of '(1 2 3) (lambda (value) (+ 1 value))) 3)))

(defmethod test-to-array ((o test-utilities))
  "Verifies to-array function works ok."
  (check 
    (is-kind-of (to-array '(1 nil 3)) 'array)
    (= 3 (length (to-array '(1 nil 3))))))

(defmethod test-to-list ((o test-utilities))
  "Verifies to-list function works ok."
  (check 
    (is-kind-of (to-list #(1 nil 3)) 'list)
    (= 3 (length (to-list #(1 nil 3))))))

(defmethod test-to-list-without-nils ((o test-utilities))
  "Verifies list-without-nils function works ok."
  (check 
    (is-kind-of (to-list-without-nils #(1 nil 3)) 'list)
    (= 2 (length (to-list-without-nils #(1 nil 3))))))


