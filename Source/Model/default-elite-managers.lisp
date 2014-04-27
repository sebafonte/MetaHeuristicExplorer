
(defun initialize-elite-managers ()
  (system-add
   ;; Single, only fitness comparer for objects
   (make-instance 'elite-manager
                  :name 'fitness-single-comparer
                  :max-size 1
                  :value-function 
                  (lambda (a best object)
                    (if (> (fitness object)
                           (fitness (best-individual a)))
                        (setf (best-individual a) object))))
   ;; Single, first fitness and then size
   (make-instance 'elite-manager
                  :name 'fitness-size-single-comparer
                  :max-size 1
                  :value-function 
                  (lambda (a best object)
                    (let ((best (best-individual a)))
                      (if (or (> (fitness object)
                                 (fitness best))
                              (and (= (fitness best) 
                                      (fitness object))
                                   (< (size (program object))
                                      (size (program best)))))
                          (setf (best-individual a) object)))))
   ;; Multi, only fitness comparer for objects
   (make-instance 'elite-manager
                  :name 'fitness-multi-comparer
                  :max-size 5
                  :value-function nil)
   ;; Multi, first fitness and then size
   (make-instance 'elite-manager
                  :name 'fitness-size-multi-comparer
                  :max-size 5
                  :value-function nil)))

