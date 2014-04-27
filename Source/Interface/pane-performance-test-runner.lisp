
(defclass pane-performance-test-runner (pane-test-runner)
  ((test-container :accessor test-container)))


(defmethod interface-class ((p pane-performance-test-runner))
  "Answer <p> interface class."
  'interface-pane-performance-test-runner)

(capi:define-interface interface-pane-performance-test-runner (interface-pane-test-runner)
  (base-interface)
  (:panes
   (pane-test-suites
    capi:option-pane
    :accessor pane-test-suites
    :items (performance-default-test-suites)
    :title "Test suit"
    :callback-type :interface-data
    :selection-callback 'refresh-tests)
   (pane-tests
    capi:multi-column-list-panel
    :accessor pane-tests
    :column-function 'get-info-test
    :columns '((:title "Name" :adjust :left :width (character 50)) 
               (:title "Status" :adjust :right :visible-min-width (character 10))
               (:title "Time" :adjust :right :visible-min-width (character 10))
               (:title "Value" :adjust :right :visible-min-width (character 10)))
    :items nil
    :pane-menu (lambda (pane object x y) 
                 (make-pane-menu pane object x y (options-menu-description-test))))
   (simple-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 26 :selected-image 26
                                :help-key "Run all process"
                                :selection-callback 'run-all-tests
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 25 :selected-image 25
                                :help-key "Run"
                                :selection-callback 'run-selected-test)
                 (make-instance 'capi:toolbar-button :image 24 :selected-image 24
                                :help-key "Stop"
                                :selection-callback 'stop-testing
                                :enabled nil)
                 (make-instance 'capi:toolbar-button :image 7 :selected-image 7
                                :help-key "Debug"
                                :selection-callback 'debug-selected-test)
                 (make-instance 'capi:toolbar-button :image 3 :selected-image 3
                                :help-key "Save results"
                                :selection-callback 'save-results))
           :selection nil))
    :callback-type :interface-data
    :title-position :frame
    :default-image-set (capi:make-general-image-set
                       :id 'global-button-icons-images
                       :image-count 6)))
  (:layouts
   (main-layout capi:column-layout '(pane-test-suites pane-tests simple-toolbar)))
  (:default-initargs
   :layout 'main-layout
   :best-width 440
   :best-height 570
   :visible-min-width 100
   :visible-min-height 100
   :title "Test suite"))

(defmethod save-results ((interface interface-pane-test-runner) &optional (data t))
  (let* ((pane (pane interface))
         (path (capi:prompt-for-file "Select target file"
                                     :filter (model-file-extension pane)
                                     :operation :save
                                     :filters (list (model-file-extension-description pane) 
                                                    (model-file-extension pane)))))
    (when path
      (save-default-model-to pane path))))

(defmethod model-file-extension-description ((pane pane-performance-test-runner)) 
  "Performance test suite result file")

(defmethod model-file-extension ((pane pane-performance-test-runner))
  "*.performance-test-result")

(defmethod save-default-model-to ((pane pane-performance-test-runner) path)
  (if (probe-file path) (delete-file path))
  (save-source-description (test-containers pane) path))