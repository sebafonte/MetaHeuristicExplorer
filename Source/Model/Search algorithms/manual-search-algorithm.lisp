(defclass manual-search-algorithm (evolutionary-algorithm)
  ((max-time :initarg :max-time :accessor max-time)
   (evaluator :initarg :evaluator :accessor evaluator)))


(defmethod initialize-properties :after ((a manual-search-algorithm))
  "Initialize <a> properties."
  (add-properties-from-values
   a
   (:name 'evaluator :label "Evaluator" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value nil :editor 'button-editor)
   (:name 'max-time :label "Max time (sec)" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 2 :editor 'number-editor :object-parameter t
    :min-value 0 :max-value 1500)))

;; #TODO: Do this with:
;;    - Use user registry as feedback
;;    - Uniques
;; 	  - Step by step configuration (or steps)
;; 
(defmethod search-loop ((a manual-search-algorithm) seed) 
  "Search method implementation for <a>." 
  nil)