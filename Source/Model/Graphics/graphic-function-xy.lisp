
(defclass graphic-function-xy (graphic)
  ((xmin :initarg :xmin :initform nil :accessor xmin)
   (xmax :initarg :xmax :initform nil :accessor xmax)
   (ymin :initarg :ymin :initform nil :accessor ymin)
   (ymax :initarg :ymax :initform nil :accessor ymax)
   (zmin :initarg :zmin :initform nil :accessor zmin)
   (zmax :initarg :zmax :initform nil :accessor zmax)))

;; #NOTE: This is from capi implementation, is obsolete now
(defmethod draw-model (pane x y width heigth container-pane (model graphic-function-xy))
  "Dibuja la curva en pane, este metodo dibuja una curva del tipo f(x) = y."
  (declare (optimize (speed 3) (compilation-speed 0) (safety 0) (debug 0)))
  (let* ((object (model container-pane))
         (xmin (xmin model))
         (xmax (xmax model))
         (ymin (ymin model))
         (ymax (ymax model))
         (factor-x (/ width (- xmax xmin)))
         (factor-y (/ heigth (- ymax ymin)))
         (points '()))
    ;; Draw limit values between x and y axis
    (gp:draw-string pane (format nil "~A" xmin) 5 (/ heigth 2) :foreground :red)
    (gp:draw-string pane (format nil "~A" xmax) (- width 27) (/ heigth 2) :foreground :red)
    (gp:draw-string pane (format nil "~A" ymin) 5 (- heigth 5) :foreground :red)
    (gp:draw-string pane (format nil "~A" ymax) 5 10 :foreground :red)
    ;; Move points from a hash table to a list
    (maphash (lambda (x y) 
               (setf points 
                     (append points 
                             (list (* factor-x x)
                                   (- heigth (* factor-y 
                                                (apply (function model) (list object x))))))))
             (log-data object))
    ;; Draw points by pairs
    (do ((i points (cddr i)))
          ((or (null i) (null (caddr i))))
      (gp:draw-lines pane 
                     (list (car i) (cadr i) (caddr i) (cadddr i))
                     :foreground (color:make-rgb 1.0 1.0 1.0)))))
