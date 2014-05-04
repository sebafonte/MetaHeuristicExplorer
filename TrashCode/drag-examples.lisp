#|
;;; Functions for a mini-state machine to handle drag & drop behaviout
;;;		- *drag-context* has the object being dragged
;;;
;;; #TODO: - Take out the last condition, i think it's unecessary now
;;;        - Use ccase
;;; 

(defvar *drag-state* 'IDLE)
(defvar *drag-selected-object*)


(defun process-press (self x y)
  (declare (ignore x))
  (declare (ignore y))
  (cond ((equal *drag-state* 'IDLE) (progn 
                                       (save-drag-source-properties self)
                                       (setf *drag-state* 'PRESSED)))
        (t nil)))

(defun process-second-press (self x y)
  (declare (ignore x) (ignore y) (ignore self))
  (cond ((equal *drag-state* 'PRESSED) (progn 
                                          (double-clicked)
                                          (setf *drag-state* 'IDLE)))
        (t nil)))

(defun process-motion (self x y)
  (declare (ignore x) (ignore y) (ignore self))
  (cond ((equal *drag-state* 'PRESSED) (progn 
                                          (relative-motion)
                                          (setf *drag-state* 'DRAGGING)))
        ((equal *drag-state* 'DRAGGING) (progn 
                                           (relative-motion-dragging)
                                           (setf *drag-state* 'DRAGGING)))
        (t nil)))

(defun process-release (self x y)
  (declare (ignore x) (ignore y))
  (cond ((equal *drag-state* 'PRESSED) (progn 
                                          (clicked)
                                          (setf *drag-state* 'IDLE)))
        ((equal *drag-state* 'DRAGGING) (progn 
                                           (finalize-drag self)
                                           (setf *drag-state* 'IDLE)))
        (t nil)))

(defun save-drag-source-properties (pane) 
  (setf *drag-selected-object* (capi:element-parent (capi:element-parent pane)))
  (print "Save mouse position."))

(defun double-clicked () 
  (print "Double click."))

(defun clicked ()
  (print "Clicked."))

(defun relative-motion () 
  (print "Moving."))

(defun relative-motion-dragging () 
  (print "Dragging."))

(defun finalize-drag (pane)
  (if *drag-selected-object*
      (set-model (capi:element-parent (capi:element-parent pane)) 
                 *drag-selected-object*))
  (print "Drag ended."))
|#