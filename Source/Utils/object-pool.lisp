
(defclass object-pool ()
  ((registry :initform (make-hash-table :test 'string=) :accessor registry)
   (current-count :initform 0 :accessor current-count)
   (max-count :initform 5 :initarg :max-count :accessor max-count)))


;; Interface
(defmethod clear ((o object-pool))
  (clrhash (registry o))
  (setf (current-count o) 0))

(defmethod get-pool-object ((o object-pool) object)
  (get-object-key o (get-key object)))

(defmethod get-pool-object-key ((o object-pool) key)
  (get-object-key o key))

(defmethod put-pool-object ((o object-pool) object)
  (let* ((key (get-key object))
         (value (get-object-key o key)))
    (unless value
      (add-object-on o key object))))

(defmethod put-pool-object-key ((o object-pool) object key)
  (let ((value (get-object-key o key)))
    (unless value
      (add-object-on o key object))))

;; Internal
(defmethod set-object ((o object-pool) key value)
  (setf (gethash key (registry o)) value))

(defmethod add-object-on ((o object-pool) key object)
  (when (>= (current-count o) (max-count o)) 
    (clear o)
    (drop-object o key))
  (let ((value object))
    (set-object o key value)
    (incf (current-count o))))

(defmethod drop-object ((o object-pool) key)
  (remhash key (registry o))
  ;; #NOTE: Assume o was deleted
  (decf (current-count o)))

(defmethod get-object-key ((o object-pool) key)
  (gethash key (registry o)))


;; Get key functions
(defmethod get-key ((obj list))
  (let ((*print-level* 64)
        (*print-length* 10000000)
        (result ""))
    (dolist (i obj)
      (setf result (format nil "~A-~A" result i)))
    result))

#|
;; Test
(let ((pool (make-instance 'object-pool)))
  ;(check 0)
  (put-pool-object pool '(1 2 3))
  ;(check 1)
  (put-pool-object pool '(1 2 3))
  ;(check 2)
  )

(let ((pool (make-instance 'object-pool)))
  (put-pool-object pool '(1 2 3))
  (put-pool-object pool '(1 2 3))
  ;(check 1)
  )

(let ((pool (make-instance 'object-pool :max-count 1)))
  (put-pool-object pool '(1 2 3))
  ;(check 1)
  (put-pool-object pool '(1 2 3))
  ;(check 1)
  )

(let ((pool (make-instance 'object-pool)))
   (put-pool-object pool '(1 2 3))
   (clear pool)
   ;(check 0)
|#

