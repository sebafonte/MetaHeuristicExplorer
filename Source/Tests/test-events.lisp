(defclass test-events (test-base-model) 
  ())


(defmethod test-event-trigger ((o test-events))
  "Verify event trigger."
  (let ((symbol (gensym))
        (value 0))
    (when-send-to symbol :changed (lambda (&rest args) (declare (ignore args)) (incf value)) nil)
    (assert (= value 0))
    (trigger symbol :changed)
    (assert (= value 1))
    (clear-event-for symbol :changed)
    (trigger symbol :changed)
    (assert (= value 1))))

(defmethod test-event-trigger-with-args ((o test-events))
  "Verify event trigger."
  (let ((symbol (gensym))
        (value 0))
    (when-send-to symbol :changed (lambda (object x y) (declare (ignore object)) (setf value (+ x y))) nil)
    (trigger symbol :changed 3 4)
    (assert (= value 7))))

(defmethod test-trigger-event-with-args ((o test-events))
  "Verify event trigger."
  (let ((symbol (gensym))
        (value 0))
    (when-send-to symbol :changed (lambda (object x y) (declare (ignore object)) (setf value (+ x y))) nil 3 4)
    (trigger symbol :changed)
    (assert (= value 7))))

(defmethod test-same-events-trigger ((o test-events))
  "Verify event trigger."
  (let ((symbol (gensym))
        (value 0))
    (when-send-to symbol :changed (lambda (object) (declare (ignore object)) (incf value)) nil)
    (when-send-to symbol :changed (lambda (object) (declare (ignore object)) (incf value)) nil)
    (trigger symbol :changed)
    (assert (= value 2))))

(defmethod test-different-events-trigger ((o test-events))
  "Verify event trigger."
  (let ((symbol (gensym))
        (value 0))
    (when-send-to symbol :changed (lambda (object) (declare (ignore object)) (incf value)) nil)
    (when-send-to symbol :changed (lambda (object) (declare (ignore object)) (incf value 2)) nil)
    (trigger symbol :changed)
    (assert (= value 3))))

(defmethod test-event-clear ((o test-events))
  "Verify event clear."
  (let ((symbol (gensym))
        (value 0))
    (when-send-to symbol :changed (lambda (&rest args) (declare (ignore args)) (incf value)) nil)
    (trigger symbol :changed)
    (assert (= value 1))))

