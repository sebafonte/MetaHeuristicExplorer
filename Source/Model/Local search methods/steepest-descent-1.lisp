
(defmethod constant-optimization-steepest-descent ((object object-in-search) (s optimization-method))
  "Optimize <object> constants using <s> steepest descent like optimization."
  (let* ((o (object object))
         (values (get-constants-information o))
         (gradient (make-array (length values)))
         (precision (precision s))
         (max-iterations (max-iterations s))
         (algorithm (algorithm (context object)))
         (delta-gradient (delta-gradient s))
         (g1)        
         (g2)
         (o1)
         (o2)
         (history)
         (alpha))
    ;; #TODO: Check if it`s necessary to evaluate object here, it should'nt be necessary
    (evaluate algorithm o)
    (let ((result 
           (block 1
             (do ((i 0 (1+ i)))
                 ((> i max-iterations) 
                  nil)
               ;; Save g1
               (setf o1 (copy o) 
                     g1 (fitness o)
                     values (get-constants-information o1))
               ;; Calculate gradient
               (calculate-gradient o1 delta-gradient algorithm values gradient)
               ;; If gradient = 0, stop
               (if (= 0 (reduce '+ (map 'vector (lambda (x) (* x x)) gradient)))
                   (return-from 1 o1))
               ;; Normalize gradient
               (setf gradient (normalize-gradient gradient))
               ;; Initialize alpha
               (setf alpha (calculate-alpha-from alpha history))
               (evaluate-with-costants o1 algorithm alpha gradient values)
               (setf o2 (copy o1) g2 (fitness o1))
               ;; Move to best solution
               (if (> g2 g1)
                   (progn
                     ;; Update history
                     (setf history (cons 'OK history))
                     (if (> (length history) 2)
                         (setf history (list (first history) (second history))))
                     ;; Update current object to o2
                     (setf o (copy o2)))
                   ;; Update history
                 (progn 
                   (setf history (cons 'ERROR history))
                   (if (> (length history) 2)
                       (setf history (list (first history) (second history))))
                   ;; Check for minimum alpha
                   (setf alpha (/ alpha 2))
                   (if (< alpha precision)
                       (return-from 1 o)))))
             ;; If no improvement has been found, answer initial object
             ;; #TODO - #TEST: Test case passing right here
             o)))
      ;; Build result object
      (make-instance 'object-in-search :object result :context (context object)))))

(defun calculate-alpha-from (alpha history)
  (if (null history)
      1
    (if (eq (first history) 'error)
        (/ alpha 2)
      (if (eq (second history) 'error)
          alpha
        (* alpha 2)))))

(defun calculate-gradient (o delta algorithm values gradient)
  (let ((variable)
        (fitness))
    (do ((var values (rest var))
         (index-var 0 (1+ index-var)))
        ((null var))
      (setf variable (car var))
      (set-default-node-value variable o)
      (setf fitness (evaluate algorithm o))
      (replace-node-value variable (+ (car variable) delta) o)
      (setf (aref gradient index-var) (- (evaluate algorithm o) fitness)))))

(defun evaluate-with-costants (o algorithm alpha gradient values)
  (do ((var values (rest var))
       (index-var 0 (1+ index-var)))
      ((null var))
    (replace-node-value (car var) (+ (caar var) (* (aref gradient index-var) alpha)) o))
  (evaluate algorithm o))

;; #TODO: Arreglar para no usar una variable especial
(defun add-constant-tree-info (node parent)
  "Add <node> constant lists to 'node-list-info special variable."
  (declare (special node-list-info))
  (if (consp node)
      (progn 
        (add-constant-tree-info (car node) node)
        (add-constant-tree-info (cdr node) node))
    (if (numberp node)
        (push (cons node (list parent)) node-list-info))))

(defmethod get-constants-information ((o entity))
  "Answer a list with parent nodes of constants on <o> program.
   NOTA: Ver formato."
  (let ((tree (program o))
        (node-list-info))
    (declare (special node-list-info))
    (add-constant-tree-info tree (program o))
    node-list-info))

(defmethod replace-node-value (node-value value (o entity))
  "Set <node-value> value to <value>."
  (setf (car (cadr node-value)) value))

(defmethod set-default-node-value (node-value (o entity))
  "Set <node-value> default value as the actual value."
  (setf (car (cadr node-value)) (car node-value)))
