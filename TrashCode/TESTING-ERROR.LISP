(defun zoom-and-exit (e) 
  "Called after error E has been trapped to cause a stack trace. It uses 
transcripts:*crash-log-stream* if it is non-nil, otherwise writes to the file *image-logname* 
It then calls the appropriate function to terminate execution (and so never returns)." 

  (setq #+excl tpl::*user-top-level* 
        #+lispworks system::*top-level-loop-function* 
        #'(lambda (&rest ignore) 
            (declare (ignore ignore)) 

            (setq #+excl tpl::*user-top-level* 
                  #+lispworks system::*top-level-loop-function* 
                  nil) ; prevent accidental recursion 

            (unwind-protect 
                 (ignore-errors 
                   (let ((*print-readably* nil) 
                         (*print-miser-width* 40) 
                         (*print-circle* t) 
                         (*print-pretty* t) 
                         #+excl (tpl:*zoom-print-level* nil) 
                         #+excl (tpl:*zoom-print-length* nil) 
                         (stream (or transcripts:*crash-log-stream* 
                                     (open *image-logname* :direction :output 
                                           :if-does-not-exist :create 
                                           :if-exists :append) 
                                     *error-output*))) 
                      
                     (let ((*terminal-io* stream) 
                           (*standard-output* stream) 
                           #+lispworks 
                           (*debug-io* stream)) 
                       (format stream "Error: ~A~%" e) 
                       #+excl 
                       (tpl:do-command "zoom" :count t :all t) 
                       #+lispworks 
                       (dbg::dbg-backtrace nil :verbose t :stream stream) 
                       (finish-output stream)))) 
              #+excl (excl:exit 0 :quiet t :no-unwind t) 
              #+lispworks (lw:quit :status 0 :confirm nil :ignore-errors-p t))))) 
