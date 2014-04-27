
(defun initialize-test-suites ()
  (system-add 
   ;; Test suite de evaluacion básica del sistema
   (make-instance 'test-suite 
                  :name 'test-suite-system
                  :test-case-classes (classes-for-tests (make-instance 'test-base-model)))
   ;; Test suite de performance de funciones básicas
   (make-instance 'test-suite 
                  :name 'test-suite-system-functions-performance
                  :test-case-classes 
                  (append
                   (classes-for-tests (make-instance 'compression-performance-test))
                   (classes-for-tests (make-instance 'task-execution-performance-test))
                   (classes-for-tests (make-instance 'operator-performance-test))
                   (classes-for-tests (make-instance 'source-code-conversion-performance-test))
                   (classes-for-tests (make-instance 'constant-optimization-performance-test))
                   (classes-for-tests (make-instance 'instanciation-performance-test))))
   (make-instance 'test-suite 
                  :name 'test-suite-default-search-tasks-performance
                  :test-case-classes (classes-for-tests (make-instance 'default-task-local-execution-performance-test)))
   (make-instance 'test-suite 
                  :name 'test-suite-environment-performance
                  :test-case-classes 
                  (append
                   (classes-for-tests (make-instance 'environment-object-transfer-performance-test))
                   (classes-for-tests (make-instance 'default-task-local-execution-performance-test))
                   (classes-for-tests (make-instance 'default-task-global-execution-performance-test))))
   (make-instance 'test-suite
                  :name 'test-suite-host-performance
                  :test-case-classes (classes-for-tests (make-instance 'host-performance-test)))))
