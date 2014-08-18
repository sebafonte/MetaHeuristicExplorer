
;;; Web interface
(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-get-languages)) message administrator stream)
  (format stream "lisp-math-function-x lisp-math-function-xy rgb-color-images")
  (force-output stream))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-create-default)) message administrator stream)
  (let* ((language-name (first (content message)))
         (result (program (subject (default-object-for language-name)))))
    (format stream "~A | ~A" result (infix-coverted-string result))
    (force-output stream)))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-create-random)) message administrator stream)
  (let* ((language (copy-cyclic (system-get (first (content message)))))
         (max-size (second (content message))))
    (setf (max-size language) max-size)
    (let ((result (create-random-from-production language '(start) max-size nil)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-crossover)) message administrator stream)
  (let* ((language (copy-cyclic (system-get (first (content message)))))
         (object-a (second (content message)))
         (object-b (third (content message)))
         (max-size (fourth (content message)))
         (operator (system-get 'crossover-cfg)))
    (setf (max-size language) max-size)
    (let ((result (directed-crossover-cfg object-a object-b language operator)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

(defmethod dispatch-message-name ((message-name (eql 'message-web-interface-mutate)) message administrator stream)
  (let* ((language (copy-cyclic (system-get (first (content message)))))
         (object (second (content message)))
         (operator (system-get 'mutate-cfg))
         (max-size (third (content message))))
    (setf (max-size language) max-size)
    (let ((result (mutate-cfg object language operator)))
      (format stream "~A | ~A" result (infix-coverted-string result))
      (force-output stream))))

;; Infix converter (#TEMP)
(defun infix-converter (o buffer)
  (if (consp o)
      (if (or (eql (first o) '+) (eql (first o) '-) (eql (first o) '*) (eql (first o) '/))
          (progn 
            ;; Operator
            (format buffer "(")
            (infix-converter (second o) buffer)
            (infix-converter (first o) buffer)
            (infix-converter (third o) buffer)
            (format buffer ")"))
        ;; Function
        (progn 
          (infix-converter (first o) buffer)
          (format buffer "(")
          (dotimes (i (length (cdr o)))
            (infix-converter (nth i (cdr o)) buffer)
            (when (< i (1- (length (cdr o))))
              (format buffer ",")))
          (format buffer ")")))
    ;; Constant or name
    (format buffer "~4$" (replace-converter o))))

(defmethod replace-converter ((o (eql '/-)))
  'divideprotected)

(defmethod replace-converter ((o t))
  o)

(defun infix-coverted-string (o)
  (with-output-to-string (stream)
    (infix-converter o stream)))

;; Default objects for testing
(defmethod default-object-for ((o (eql 'lisp-math-function-xy)))
  (random-element
   (list (system-get 'function-xy-default-4)
         (system-get 'function-xy-default-6)
         (system-get 'function-xy-default-7)
         (system-get 'function-xy-default-8)
         (system-get 'function-xy-default-9)
         (system-get 'function-xy-default-10))))

(defmethod default-object-for ((o (eql 'rgb-color-images)))
  (random-element
   (list (system-get 'rgb-default-1)
         (system-get 'rgb-default-2)
         (system-get 'rgb-default-3)
         (system-get 'rgb-default-4)
         (system-get 'rgb-default-5))))