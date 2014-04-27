
(defun initialize-default-breeders ()
  (system-add 
   ;; Default breeders for evolvable algorithms
   (make-instance 'new-population-breeder
                  :name 'full-population-breeding
                  :description 'new-population-breeder)
   (make-instance 'existing-population-breeder
                  :name 'partial-breeding
                  :description 'existing-population-breeder)))

(defun default-breeders ()
  (list (system-get-subject-copy 'full-population-breeding)
        (system-get-subject-copy 'partial-breeding)))