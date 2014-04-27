(defclass test-linear-ordering (test-base-model)
  ())


#|
;; #TEST:
(setf (matrix oo) (lop-matrix-from (default-array-description-marti-1))
      (initial-matrix oo) (copy (lop-matrix-from (default-array-description-marti-1))))


;; #TODO: Move to tests
(defun default-array-description-marti-1 ()
  '(2 4 4 7 4))

(defun default-array-description-marti-1 ()
  '(3 1 2 9 7 1 15 9 2 1))

(defun default-array-description-marti-1 ()
  '(3 1 2 9 15 1 7 8 9 1)))

(defun default-array-description-marti-1 ()
  '(4 0 0 0 0 4 7 0 0 0 9 0 0 1 7 1 0)))

(defun default-array-description-marti-1 ()
  '(3 9 0 0 0 9 0 9 0 0 ))
|#
