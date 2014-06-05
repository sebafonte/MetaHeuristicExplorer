
;; #TODO: Refactor with functions below
(defun text-input-pane-integer (pane)
  (let* ((text (capi:text-input-pane-text pane))
         (integer (parse-integer text :junk-allowed t)))
    (or (and (integerp integer) integer)
        (values nil t))))

(defun prompt-for-plusp-integer (message)
  (capi:popup-confirmer
   (make-instance 'capi:text-input-pane :change-callback :redisplay-interface :callback 'capi:exit-confirmer)
   message
   :value-function 'text-input-pane-integer
   :ok-check 'plusp))

(defun prompt-for-sub-expression (message)
  (multiple-value-bind (result action)
      (capi:popup-confirmer 
       (make-instance 'capi:text-input-pane :change-callback :redisplay-interface :callback 'capi:exit-confirmer)
       message
       :value-function (lambda (pane) (capi:text-input-pane-text pane))
       :ok-check (lambda (pane) (declare (ignore pane)) t))
    (values action (read-from-string result))))

(defun absolute-geometry-values (interface)
  "Answer the values for <interface> with respect to main top level interface."
  (if (capi:top-level-interface-p interface)
      (multiple-value-bind (x y h w)
          (capi:top-level-interface-geometry interface)
        (list x1 y1 h1 w1))
    (let ((top (multiple-value-bind (x y) (capi:top-level-interface-geometry (capi:top-level-interface interface)) (list x y))))
      (capi:with-geometry interface
        (list (+ capi:%x% (car top))
              (+ capi:%y% (cadr top))
              capi:%height% 
              capi:%width%)))))
