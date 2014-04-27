
(defclass adfs-manager (abstract-subroutine-manager)
  ((max-functions :initarg :max-functions :accessor max-functions)
   (max-arguments :initarg :max-arguments :accessor max-arguments)
   (language :initarg :language :accessor language)
   (recursion-strategy :initarg :recursion-strategy :accessor recursion-strategy)))


(defmethod initialize-properties :after ((o adfs-manager))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :default-value "ADFs subroutine manager" :data-type 'symbol :editor 'text-editor)
   (:name 'recursion-strategy :label "Recursion strategy" :accessor-type 'accessor-accessor-type 
    :default-value nil :data-type 'symbol :editor 'text-editor 
    :possible-values '(no-recursion hierarchical-recursion))))


;;; ADF Example Form #1
;;;
;; (list (((adf-0 1) (+ arg0))
;:        ((adf-1 2) (* arg0 arg1)))
;;       (adf-0 (adf-1 1 2)))

;;; ADF Example Form #2
;;;
;; (progn (defun (adf-0 (a) (+ a)))
;:        (defun (adf-1 (a b) (* a b)))
;;        (values (adf-0 (adf-1 1 2))))


(defmethod ensure-not-recurrent ((manager adfs-manager) object)
  (ensure-not-recurrent-using manager object (recursion-strategy object)))

(defmethod ensure-not-recurrent-using ((manager adfs-manager) object (strategy (eql 'no-recursion)))
  nil)

(defmethod ensure-not-recurrent-using ((manager adfs-manager) object (strategy (eql 'hierarchical-recursion)))
  nil)

(defmethod replace-adfs-bigger-than (object value)
  nil)

;; ADF creation function

;; ADF correction function



