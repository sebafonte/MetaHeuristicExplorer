
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

(defmacro protect-interface-creation (args)
  `(restart-case
       (handler-bind ((error #'(lambda (c)
                                 (declare (ignore c))
                                 (invoke-restart 'my-restart nil))))
         (progn ,@args))
     (my-restart (&optional v) v)))
