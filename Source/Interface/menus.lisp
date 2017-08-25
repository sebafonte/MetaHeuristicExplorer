
(defun options-menu-description-subtasks (title menu-description-list pane)
  "Answer a menu description with <title> for <pane>."
  (declare (ignore title))
  (let ((description-list))
    (dolist (item menu-description-list)
      (appendf 
       description-list
       (list 
        (if (consp (cadr item))
            (make-instance 'capi:menu
                           :title (car item)
                           :items (options-menu-description-subtasks (car item) (cdr item) pane))
          (let* ((enable-selector (caddr item))
                 (enabled-value (if (equal enable-selector :disabled) 
                                    (lambda (object) 
                                      (declare (ignore object))
                                      nil)
                                  (if (or (functionp enable-selector)
                                          (and (symbolp enable-selector) enable-selector))
                                      (lambda (object) 
                                        (declare (ignore object)) 
                                        (funcall enable-selector pane))
                                    enable-selector))))
            (make-instance 'capi:menu-item 
                           :text (car item) 
                           :callback (cadr item) 
                           :callback-type :interface-data
                           :enabled-function enabled-value))))))
    description-list))