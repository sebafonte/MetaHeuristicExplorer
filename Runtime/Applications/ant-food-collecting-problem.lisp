(defparameter *ant-food-collector-expression-tokens* nil)

#|
;;;; #TODO: Subclassify
(defclass tree-language (language)
  ...)
|#   
  
;;; Language
(defun ant-food-collector-default-functions-info ()
  '((prog2 2) (prog3 3) (if-else 3)))

;; #CHECK :functions y :terminals keys should be specified only 
 
;;; Entity object
(defclass food-collector-ant (entity)
    ())
       
#|    
;; Language    
(system-add
   (make-instance 'tree-language 
                  :name 'ant-food-collector-language
                  :functions (ant-food-collector-default-functions-info)
                  :terminals '(:turn-left :turn-right :move)
                  :tokens *ant-food-collector-expression-tokens*
                  :valid-new-expresion-function 'create-new-random-valid
                  :operators (default-genetic-operators-probability-ant-food-collector)))
|#

;;; Evaluator 
(defclass food-collector-ant-evaluator ()    
    ((initial-position :initarg :initial-position :accessor initial-position)
     (initial-direction :initarg :initial-direction :accessor initial-direction)))
    
    
(defmethod fitness-value ((e food-collector-ant-evaluator))
    (+ (- 1000 steps) food))

(defmethod evaluate ((e food-collector-ant-evaluator) (o food-collector-ant))
    (let ((initial-position ())
          (initial-direction ())
          (grid ())
          (food 0)
          (steps 0)
          (program (compiled-program o)))
        nil))

;; Graphics
;; #TODO: 
(defmethod draw-function ((e food-collector-ant-evaluator) (o food-collector-ant))
    nil)
