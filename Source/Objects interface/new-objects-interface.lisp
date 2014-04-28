(defclass example-object (entity)
  ())

  
(defmethod initialize-properties-for ((o example-object) (target search-task))
  "Initialize properties for <o> in <target>."
  nil)

(defmethod possible-fitness-functions ((o example-object))
  "Answer <o> possible fitness-functions."
  '(evaluate-distance ))

(defmethod initialize-fitness ((subtask search-task) (o example-object))
  "Initialize <o> fitness on <subtask>."
  nil)

(defmethod evaluate-distance ((a search-algorithm) (o example-object))
  "Evaluate <o> fitness depending on the distance to a specific objetive."
  0)

(defmethod draw-object-background ((o example-object) subtask interface width heigth)
  "Draw <o> on <interface>."
  (loop for ix from 0 to width by 1 do  
    (loop for iy from 0 to height by 1 do  
      (gp:draw-point interface ix iy :foreground (color:make-gray 0)))))

;;; #TODO: Add this
;; (defmethod default-fixed-constants ((o example-object))
;;  "Answer fixed constant set for <o>.
;;  '(0 1 2 3 4 5 6 7 8 9)

(defmethod default-possible-operators ((o example-object))
  "Answer possible operator list for <o> program. 
   NOTA: Arity is a fixed value."
  '((+ 2) (- 2) (* 2) (/- 2) (sin 1) (cos 1) (abs 1)))

(defmethod default-possible-terminals ((o example-object))
  "Answer a list with possible terminals for <o>."
  '(x y :constant-int))

(defmethod variables-default ((o example-object))
  "Answer a list with possible variables for <o>."
  '(x))

(defmethod grammar-default ((o example-object))
  "Answer default grammar class for <o>."
  'grammar-n-variables)

(defmethod grammars-posibles-default ((o example-object))
  "Answer default grammar class names <o>."
  '(grammar-n-variables))

(defmethod drawablep ((o example-object))
  "Answer whether o can be displayed on the GUI."
  nil)
