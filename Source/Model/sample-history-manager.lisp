
(defclass sample-history-manager ()
  ((current-index :accessor current-index)
   (pages :initarg :pages :initform nil :accessor pages)
   (level :initarg :level :initform 3 :accessor level)))


(defmethod initialize-instance :after ((o sample-history-manager) &rest args)
  (set-to-last o))

(defmethod set-to-last ((o sample-history-manager))
  (setf (current-index o) (1- (length (pages o)))))

(defmethod add-page ((o sample-history-manager) page)
  (appendf (pages o) (list page))
  (set-to-last o)
  (when (> (length (pages o)) (level o))
    (setf (pages o) (cdr (pages o)))
    (decf (current-index o))))

(defmethod next ((o sample-history-manager))
  (when (and (< (current-index o) (1- (level o)))
             (< (current-index o) (1- (length (pages o)))))
    (incf (current-index o)))
  (current-page o))

(defmethod back ((o sample-history-manager))
  (when (> (current-index o) 0)
    (decf (current-index o)))
  (current-page o))

(defmethod current-page ((o sample-history-manager))
  (if (>= (current-index o) 0)
      (nth (current-index o) (pages o))
    nil))  

(defmethod last-page ((o sample-history-manager))
  (last (pages o)))

(defmethod first-page ((o sample-history-manager))
  (first (pages o)))

(defmethod first-page-p ((o sample-history-manager))
  (zerop (current-index o)))

(defmethod last-page-p ((o sample-history-manager))
  (= (length (pages o)) (1+ (current-index o))))

(defmethod empty-p ((o sample-history-manager))
  (null (pages o)))


#|
;; #TODO: Move to test method
(let ((o (make-instance 'sample-history-manager)))
  (add-page o '(1 2 3))
  (add-page o '(4 5 6))
  (add-page o '(7 8 9))
  (print (current-page o))
  (back o)
  (print (current-page o))
  (back o)
  (print (current-page o))
  (back o)
  (print (current-page o))
  (back o)
  (print (current-page o))
  (next o)
  (print (current-page o))
  (next o)
  (print (current-page o))
  (next o)
  (print (current-page o))
  (next o)
  (print (current-page o)))
|#