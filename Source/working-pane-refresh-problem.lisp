
(defun refresh-subtasks-items (list subtasks)
  (let ((old-selection (capi:choice-selected-item subtasks)))
    (when (not (equals (capi:collection-items subtasks) list))
      (setf (capi:collection-items subtasks) list))
    (when (includes (capi:choice-selected-item subtasks) old-selection)
      (setf (capi:choice-selected-item subtasks) old-selection))))

(defun refresh-subtasks-redraw (subtasks)
  (capi:map-collection-items 
   subtasks
   (lambda (item) 
     (capi:redisplay-collection-item subtasks item))))
