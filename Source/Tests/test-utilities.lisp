(defclass test-utilities (test-base-model) ())


(defmethod test-maximum-of-list ((o test-utilities))
  (check 
    (= (maximum-of '(1 2 3) (lambda (value) (+ 1 value))) 3)
    (= (maximum-of '(1 2 3) (lambda (value) (+ 1 value))) 3)))

#|
(defmethod test-to-array ((o test-utilities))
  "Verifies to-array function works ok."
  nil)

(defmethod test-to-list ((o test-utilities))
  "Verifies to-list function works ok."
  nil)

(defmethod test-to-list-without-nils ((o test-utilities))
  "Verifies list-without-nils function works ok."
  nil)

(defmethod test-list-to-string ((o test-utilities))
  "Verifies list-to-string function works ok."
  nil)

(defmethod test-load-save-to-file ((o test-utilities))
  "Verifies save-to-file and save-to-file function are working ok."
  nil)
|#

