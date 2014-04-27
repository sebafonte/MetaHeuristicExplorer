
(defclass pane-test-runner (base-pane)
  ((test-containers :accessor test-containers)))


(defmethod initialize-interface :after ((p pane-test-runner))
  "Initialize <p> interface."
  (initialize-test-containers p)
  (update-test-list (interface p)))

(defmethod initialize-test-containers ((p pane-test-runner))
  "Initialize <p> test containers."
  (let ((selected-suite (capi:choice-selected-item (pane-test-suites (interface p)))))
    (setf (test-containers p) (test-containers selected-suite))))

(defmethod update-test-list ((interface interface-pane-test-runner))
  "Updates <interface> test list."
  (initialize-test-containers (pane interface))
  (update-pane-test-list interface))

(defmethod update-pane-test-list-conserve-selection ((i interface-pane-test-runner))
  (let ((selection-index (capi:choice-selection (pane-tests i))))
    (update-pane-test-list i)
    (capi:apply-in-pane-process
     (pane-tests i) #'(setf capi:choice-selection) selection-index (pane-tests i))))

(defmethod update-pane-test-list ((i interface-pane-test-runner))
  (capi:apply-in-pane-process
   (pane-tests i) #'(setf capi:collection-items) (test-containers (pane i)) (pane-tests i)))

(defmethod interface-class ((p pane-test-runner))
  "Answer <p> interface class."
  'interface-pane-test-runner)

(defmethod run-tests ((object pane-test-runner) &optional (data t))
  "Executes tests of <object>."
  (dolist (test (test-containers object))
    (run-test test)
    (update-pane-test-list-conserve-selection (interface object))))

(defmethod stop-run-case-withtesting ((p pane-test-runner))
  "Stop the execution of tests on <p>."
  nil)

(defmethod refresh-tests (interface data)
  (declare (ignore data))
  (update-test-list interface))

(defmethod run-all-tests ((object interface-pane-test-runner) &optional (data t))
  (run-tests (pane object)))

(defmethod run-selected-test ((interface interface-pane-test-runner) &optional (data t))
  (let ((selection (capi:choice-selected-item (pane-tests interface))))
    (when selection
      (run-test selection)
      (update-pane-test-list-conserve-selection interface))))

;; #TODO: Habría que poner un ensure (hacerlo) para que se grafique siempre la interface
(defmethod debug-selected-test ((interface interface-pane-test-runner) &optional (data t))
  (let ((selection (capi:choice-selected-item (pane-tests interface))))
    (when selection
      (debug-test selection)
      (update-pane-test-list-conserve-selection interface))))

(capi:define-interface interface-pane-test-runner (base-interface)
  (base-interface)
  (:panes
   (pane-test-suites
    capi:option-pane
    :accessor pane-test-suites
    :items (behaviour-default-test-suites)
    :title "Test suit"
    :callback-type :interface-data
    :selection-callback 'refresh-tests)
   (pane-tests
    capi:multi-column-list-panel
    :accessor pane-tests
    :column-function 'get-info-test
    :columns '((:title "Name" :adjust :left :width (character 50)) 
               (:title "Status" :adjust :right :visible-min-width (character 10))
               (:title "Time" :adjust :right :visible-min-width (character 10)))
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
                                :selection-callback 'debug-selected-test))
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
   :best-width 420
   :best-height 570
   :visible-min-width 100
   :visible-min-height 100
   :title "Test suite"))

(defmethod options-menu-description-test ()
  '(("Run selected" run-selected-test)
    ("Debug selected" debug-selected-test)))


