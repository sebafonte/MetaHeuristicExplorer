
(defclass array-mutation (unary-genetic-operator)
  ((mutation-points-number :initarg :mutation-points-number :accessor mutation-points-number)
   (consecutive :initarg :consecutive :accessor consecutive)))


(defmethod initialize-properties :after ((o array-mutation))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'mutation-points-number :label "Points number" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 1 :max-value 10000 :default-value 1 :editor 'function-editor)))

;; #TODO: 
(defmethod operate ((o array-mutation) language genes)
  (let* ((gen (copy (first genes))))
    (dotimes (i (mutation-points-number o))
      (let ((point-index (random (length gen))))
        (setf (aref point-index gen)
              (gene-mutate (aref point-index gen) language))))
    gen))

#|
;; #TODO:     
(defun gene-mutate (gene language)
  "Mutate <gene> of <language>."
  nil)
|#

