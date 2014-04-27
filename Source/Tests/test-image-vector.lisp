(defclass test-image-vector (test-base-model) ())


(defmethod test-image-vector-add ()
  "Test arithmetic opertors on image-vector."
  (let* ((a (make-instance 'image-vector-3d :x 1 :y 1 :z 1))
         (b (make-instance 'image-vector-3d :x 1 :y 2 :z 3))
         (sum-ab (vec-+ a b))
         (sum-a2 (vec-+ a 2))
         (sum-2b (vec-+ 2 b)))
      (check (and (= (x sum-ab) 2) (= (y sum-ab) 3) (= (z sum-ab) 4)))
      (check (and (= (x sum-a2) 3) (= (y sum-a2) 3) (= (z sum-a2) 3)))
      (check (and (= (x sum-2b) 3) (= (y sum-2b) 4) (= (x sum-2b) 5)))))

(defmethod test-image-vector-accessors ()
  "Test image-vector accessors."
  (let ((vector (make-instance 'image-vector-3d :x 1 :y 2 :z 3)))
    (check (and (= (x vector) 1) 
                (= (y vector) 2) 
                (= (z vector) 3)))))

#|
;; #TODO
(defmethod test-vec-crop ((o test-image-vector))
  "Test wether associated functions with image-vectors are working ok."
  nil)

(defmethod test-vec-abs ((o test-image-vector))
  "Test wether associated functions with image-vectors are working ok."
  nil)

(defmethod test-image-vector-3d-basic-operation ((o test-image-vector))
  "Test basic operations work ok with image-vector-3d instances."
  nil)

(defmethod test-image-vector-3d-equality ((o test-image-vector))
  "Test equality with image-vector-3d objects is working ok."
  nil)
|#