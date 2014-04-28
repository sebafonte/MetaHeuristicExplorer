
(defsystem ide-problem-example 
  (:package "USER"
   :default-pathname (pathname-location (current-pathname))
   :default-type :lisp-file)
  :members ("utilities.lisp"
            "interface.lisp"
            "base-model.lisp"
            "base-pane.lisp"
            "pane-principal.lisp"
            "pane-tasks.lisp")
  :rules 
  ((:in-order-to :compile :all
    (:requires (:load :previous)))))

(setf *system* (compile-system 'ide-problem-example :force t :load t))
(register-capi-button-icons)
(open-pane (make-instance 'pane-principal))
