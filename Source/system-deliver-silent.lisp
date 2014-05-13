
;; #TODO: Move to defsystem parameter
(proclaim '(optimize (speed 3) (space 0) (debug 0)))
(proclaim '(optimize (safety 0) (speed 3)))

(use-package '(parsergen))

(defvar *system*)
(defvar *main-pane*)
(defvar *base-pathname* (pathname-location (current-pathname)))

(load (merge-pathnames "asdf.lisp" *base-pathname*))
(push (merge-pathnames "Salza2\\salza2-2.0.8\\" *base-pathname*) asdf:*central-registry*)
(asdf:operate 'asdf:load-op :salza2)
(push (merge-pathnames "Chipz\\chipz_0.7.4\\" *base-pathname*) asdf:*central-registry*)
(asdf:operate 'asdf:load-op :chipz)
(push (merge-pathnames "s-base64\\" *base-pathname*) asdf:*central-registry*)
(asdf:operate 'asdf:load-op :s-base64)
(load (merge-pathnames "OpenGL\\host.lisp" *base-pathname*))
(load (merge-pathnames "utilities.lisp" *base-pathname*))
(load (merge-pathnames "OpenGL\\load-opengl.lisp" *base-pathname*))

(defsystem gp-tool 
  (:package "USER"
   :default-pathname *base-pathname*
   :default-type :lisp-file)
  :members (;; Globals
            "asdf.lisp"
            "globals.lisp"
            ;; Utilities
            "list-utilities.lisp"
            "utilities.lisp" 
            "compression-utilities.lisp" 
            "perlin-noise.lisp"
            "dft-and-fft.lisp"
            "matching.lisp"
            "list-selection.lisp"
            "node-selection.lisp"
            "pattern-simplification.lisp"
            "copy.lisp"
            ;; OpenGL
            "OpenGL\\host.lisp"
            "OpenGL\\load-opengl.lisp"
            "OpenGL\\opengl-utilities.lisp"
            ;; Events
            "Model\\Events\\events.lisp"
            ;; OpenCL
            "OpenCL\\ocl.lisp"
            "OpenCL\\ocl-utility.lisp"
            ;; Properties
            "Model\\Properties\\object-with-properties.lisp"
            "Model\\Properties\\property.lisp"
            ;; Model
            "Model\\base-model.lisp"
            "Model\\image-vector.lisp"
            "Model\\Properties\\editable-object-wrappers.lisp"
            "Model\\Properties\\property-editor.lisp"
            "Model\\entity.lisp"
            "Model\\object-in-search.lisp"
            "Model\\genotype.lisp"
            "Model\\population.lisp"
            "Model\\elite-manager.lisp"
            "Model\\sample-history-manager.lisp"
            ;; Entities
            "Model\\Entities\\entity-function.lisp"
            "Model\\Entities\\entity-function-maximization.lisp"
            "Model\\Entities\\entity-function-x.lisp"
            "Model\\Entities\\entity-function-x-y.lisp"
            "Model\\Entities\\entity-image-bw.lisp"
            "Model\\Entities\\entity-image-rgb.lisp"
            "Model\\Entities\\entity-texture-deformation.lisp"
            "Model\\Entities\\entity-texture-deformation-enclosure.lisp"
            "Model\\Entities\\entity-texture-deformation-separate.lisp"
            "Model\\Entities\\entity-image-seamless.lisp"
            "Model\\Entities\\entity-pattern.lisp"
            "Model\\Entities\\entity-linear-ordering.lisp"
            "Model\\Entities\\entity-linear-ordering-list.lisp"
            "Model\\Entities\\entity-sample-vrp.lisp"
            "Model\\Languages\\entity-language.lisp"
            "Model\\Languages\\entity-language-free-functions.lisp"
            ;; Search algorithms
            "Model\\Search algorithms\\search-algorithm.lisp"
            "Model\\Search algorithms\\random-tree-creation-strategy.lisp"
            "Model\\Search algorithms\\simplification-strategy.lisp"
            "Model\\Search algorithms\\evolutionary-algorithm.lisp"
            "Model\\Search algorithms\\generational-algorithm.lisp"
            "Model\\Search algorithms\\steady-state-algorithm.lisp"
            "Model\\Search algorithms\\mu-lambda-evolutionary-strategy-algorithm.lisp"
            "Model\\Search algorithms\\nsga.lisp"
            "Model\\Search algorithms\\nsga-ii.lisp"
            "Model\\Search algorithms\\manual-search-algorithm.lisp"
            "Model\\Search algorithms\\vrp-grasp-els-algorithm.lisp"
            "Model\\Search algorithms\\configurable-search-algorithm.lisp"
            "Model\\Search algorithms\\breeders.lisp"
            ;; Genetic operators
            "Model\\population-generators.lisp"
            "Model\\constant-factory.lisp"
            "Model\\fixed-set-constants-factory.lisp"
            "Model\\ephemeral-random-constants-factory.lisp"
            "Model\\random-lisp-math-expression-creation.lisp"
            "Model\\Operators\\tree-utilities.lisp"
            "Model\\Operators\\genetic-operators.lisp"
            "Model\\Operators\\genetic-operators-test.lisp"
            "Model\\Operators\\genetic-operators-gep.lisp"
            "Model\\Operators\\genetic-operators-koza.lisp"
            "Model\\Operators\\crossover-cfg.lisp"
            "Model\\Operators\\mutate-cfg.lisp"
            "Model\\Operators\\branch-delete-cfg.lisp"
            "Model\\Operators\\cfg-node-selection-probability-functions.lisp"
            "Model\\Operators\\genetic-operator.lisp"
            "Model\\Operators\\crossovers.lisp"
            "Model\\Operators\\subtree-crossover.lisp"
            "Model\\Operators\\mutations.lisp"
            "Model\\Operators\\branch-delete.lisp"
            "Model\\Operators\\one-point-array-operators.lisp"
            "Model\\Operators\\array-mutation.lisp"
            "Model\\Operators\\sample-vrp-operations.lisp"
            "Model\\Operators\\sample-vrp-crossover-ox.lisp"
            ;; Languages
            "Model\\Languages\\parsers.lisp"
            "Model\\Languages\\parser-lisp-expresion-math-functions.lisp"
            "Model\\Languages\\parser-lisp-math-expression-with-subroutines.lisp"
            "Model\\Languages\\parser-polynomial-expression.lisp"
            "Model\\Languages\\parser-infix-math-expression.lisp"
            "Model\\Languages\\parser-rgb-images.lisp"
            "Model\\Languages\\parser-rgb-images-with-subroutines.lisp"
            "Model\\Languages\\parser-cl-expression.lisp"
            "Model\\Languages\\grammar.lisp"
            "Model\\Languages\\context-free-grammar.lisp"
            "Model\\Languages\\context-free-grammar-with-subroutines.lisp"
            "Model\\Languages\\search-task-grammar.lisp"
            "Model\\Languages\\search-algorithm-grammar.lisp"
            "Model\\Languages\\language.lisp"
            "Model\\Languages\\binary-language.lisp"
            "Model\\Languages\\tree-language.lisp"
            "Model\\Languages\\search-algorithm-language.lisp"
            "Model\\Languages\\cfg-tree-language.lisp"
            "Model\\Languages\\vrp-list-language.lisp"
            ;; Subroutine encapsulation
            "Model\\Subroutines\\abstract-subroutine-manager.lisp"            
            "Model\\Subroutines\\subroutine-replacement-strategy.lisp"
            "Model\\Subroutines\\subroutine-scoring-strategy.lisp"
            "Model\\Subroutines\\compression-subroutine-manager.lisp"
            "Model\\Subroutines\\subroutine-transfer-strategy.lisp"
            "Model\\Languages\\compression-tree-language.lisp"
            "Model\\Operators\\subroutine-compression-operator.lisp"
            "Model\\Operators\\subroutine-depth-compression-operator.lisp"
            "Model\\Subroutines\\adf-manager.lisp"
            "Model\\Subroutines\\adf-operators.lisp"
            "Model\\Languages\\adf-tree-language.lisp"
            ;; Registry
            "Model\\selection-context.lisp"
            "Model\\object-registry.lisp"
            ;; Algorithm strategies
            "Model\\selection-methods.lisp"
            "Model\\optimization-target.lisp"
            "Model\\optimization-strategy.lisp"
            "Model\\best-of-population-optimization-target.lisp"
            "Model\\random-population-optimization-target.lisp"
            "Model\\population-optimization-target.lisp"
            "Model\\optimization-method.lisp"
            "Model\\composite-optimization-method.lisp"
            "Model\\Local search methods\\steepest-descent-optimization.lisp"
            "Model\\Local search methods\\pattern-simplification-optimization.lisp"
            "Model\\Local search methods\\local-optimizations.lisp"
            ;; Search tasks
            "Model\\Tasks and search distribution\\search-task.lisp"
            "Model\\Tasks and search distribution\\print-tasks.lisp"
            ;; Fitness evaluators
            "Model\\Fitness evaluators\\entity-evaluator.lisp"       
            "Model\\Fitness evaluators\\entity-function-maximization-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-function-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-function-x-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-function-x-values-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-function-xy-values-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-function-x-y-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-function-x-y-poderated-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-sucession-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-image-bw-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-image-rgb-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-image-seamless-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-linear-ordering-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-sample-vrp-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-cvrp-evaluator.lisp"
            "Model\\Fitness evaluators\\entity-dvrp-evaluator.lisp"
            "OpenCL\\opencl-evaluator.lisp"
            "OpenCL\\entity-function-x-cl-evaluator.lisp"
            "OpenCL\\entity-function-x-y-cl-evaluator.lisp"
            "Model\\grasp-vrp-heuristic.lisp"
            "Model\\clark-and-wright.lisp"
            "Model\\insertion-heuristic.lisp"
            "Model\\sweep-heuristic.lisp"
            "Model\\randomized-nearest-neighbor-generator.lisp"
            "Model\\Fitness evaluators\\search-task-objetive-fitness-evaluator.lisp"
            "Model\\Fitness evaluators\\search-algorithm-objetive-fitness-evaluator.lisp"
            "Model\\system-environment.lisp"            
            "Model\\Defaults\\default-elite-managers.lisp"
            "Model\\Defaults\\default-fitness-evaluators.lisp"
            ;; Tasks and search distribution
            "Model\\Tasks and search distribution\\tcp-message.lisp"
            "Model\\Tasks and search distribution\\connection-descriptor.lisp"
            "Model\\Tasks and search distribution\\connection-administrator.lisp"
            "Model\\Tasks and search distribution\\task-builder.lisp"
            "Model\\Tasks and search distribution\\task-creator.lisp"
            "Model\\Tasks and search distribution\\n-runs-task-builder.lisp"
            "Model\\Tasks and search distribution\\task-planifier.lisp"
            "Model\\Tasks and search distribution\\running-image-planifier.lisp"
            "Model\\Tasks and search distribution\\local-planifier.lisp"
            "Model\\Tasks and search distribution\\remote-planifier.lisp"
            "Model\\Tasks and search distribution\\equitative-planifier.lisp"
            "Model\\Tasks and search distribution\\balanced-planifier.lisp"
            "Model\\Tasks and search distribution\\n-iterator.lisp"
            "Model\\Tasks and search distribution\\population-size-variation-iterator.lisp"
            "Model\\Tasks and search distribution\\distributed-environment.lisp"
            "Model\\Tasks and search distribution\\benchmarking.lisp"
            "Model\\Tasks and search distribution\\distributed-environment-initialization.lisp"
            "Model\\Tasks and search distribution\\main-server-informer.lisp"
            ;; Benchmarking
            "Model\\Benchmark\\base-benchmark.lisp"
            "Model\\Benchmark\\task-benchmark.lisp"
            "Model\\Benchmark\\algorithm-benchmark.lisp"
            "Model\\Benchmark\\gp-crossover-tune-benchmark.lisp"
            ;; Logging
            "Model\\log-inspector.lisp"
            "Model\\log-data-container.lisp"
            "Model\\log-data.lisp"
            ;; Default objects
            "Model\\Defaults\\default-genetic-operators.lisp"
            "Model\\Defaults\\default-constant-factory.lisp"
            "Model\\Defaults\\default-grammars.lisp"
            "Model\\Defaults\\default-languages.lisp"
            "Model\\Defaults\\default-breeders.lisp"
            "Model\\Defaults\\default-algorithms.lisp"
            "Model\\Defaults\\default-optimization-targets.lisp"
            "Model\\Defaults\\default-optimization-methods.lisp"
            "Model\\Defaults\\default-optimization-strategies.lisp"
            ;; GUI
            "Interface\\pane-positioner.lisp"
            "Interface\\redrawing-with-pixmap.lisp"
            ;; OpenGL panes
            "opengl\\opengl-panes.lisp"
            "opengl\\opengl-code.lisp"
            ;; Editors
            "Interface\\editable-parameter-editor.lisp"
            "Model\\Properties\\Editors\\configurable-copy-list-editor.lisp"
            "Model\\Properties\\Editors\\compound-object-from-list-editor.lisp"
            "Interface\\interface-functions.lisp"
            "Interface\\menus.lisp"
            ;; Graphs
            "Model\\Graphics\\graphic.lisp"
            "Model\\Graphics\\graphic-function-r-r.lisp"
            "Model\\Graphics\\graphic-population-diversity.lisp"
            "Model\\Graphics\\graphic-histogram.lisp"
            "Model\\Graphics\\graphic-spectrum-x-y.lisp"
            "Model\\Graphics\\graphic-property-map.lisp"
            ;; Configuration
            "Model\\registrable-object-wrapper.lisp"
            "Configuration\\default-objects.lisp"
            "visualization-modes.lisp"
            "system-configuration.lisp"
            ;; Panes
            "Interface\\base-pane.lisp"
            "Interface\\pane-buffer.lisp"
            "Interface\\pane-feedback.lisp"
            "Interface\\pane-explorer.lisp"
            "Interface\\node-data.lisp"
            "Interface\\pane-graphic.lisp"
            "Interface\\pane-distributed-environment-configuration.lisp"
            "Interface\\pane-popup-menu.lisp"
            "Interface\\pane-principal.lisp"
            "Interface\\pane-search-tasks.lisp"
            "Interface\\pane-subtasks.lisp"
            "Interface\\pane-task-creator.lisp"
            "Interface\\pane-map.lisp"
            "Interface\\drag-and-drop.lisp"
            "Interface\\pane-test-runner.lisp"
            "Interface\\pane-performance-test-runner.lisp"
            "Interface\\pane-performance-test-runner-comparer.lisp"
            "Interface\\pane-editor-lisp.lisp"
            "Interface\\pane-editor-entity.lisp"
            "Interface\\pane-editor-entity-explorer.lisp"
            "Interface\\pane-choice-select.lisp"
            "Interface\\pane-description.lisp"
            "Interface\\graphics-refresh.lisp"
            ;; Interfaces
            "Interface\\interface-pane-editor-properties.lisp"
            "Interface\\interface-pane-editor-entity.lisp"
            "Interface\\interface-pane-editor-entity-image.lisp"
            "Interface\\interface-pane-editor-entity-introns.lisp"
            "Interface\\interface-pane-editor-entity-base.lisp"
            "Interface\\interface-pane-editor-entity-explorer.lisp"
            "Interface\\interface-pane-editor-entity-image-opengl.lisp"
            "Interface\\interface-pane-editor-entity-explorer-opengl.lisp"
            ;; Source code extraction
            "Model\\Tasks and search distribution\\source-code-description.lisp"
            ;; OpenGL drawing
            "Interface\\texture.lisp"
            "Interface\\gl-texture-manager.lisp"
            "OpenGL\\objects-drawing.lisp"
            "OpenGL\\graphic-objects-update.lisp"
            ;; Initialization
            "initialization.lisp"
            ;; Local search methods
            "Model\\Local search methods\\steepest-descent-1.lisp"
            ;; Other
            "Model\\create-child.lisp"
            "copy-cyclic.lisp"
            "command-line-functions.lisp"
            "Model\\Reporting\\reporting.lisp"
            "Model\\Reporting\\reporting-tool.lisp"
            "Model\\Reporting\\text-stream-reporting.lisp"
            ;; Applications load
            "Applications\\load-file.lisp"
            ;; Testing
            "unit-test-framework.lisp"
            "Test unit\\test-resource.lisp"
            "Test unit\\test-container.lisp"
            "Test unit\\test-result.lisp"
            "Test unit\\test-case.lisp"
            "Test unit\\test-suite.lisp"
            "Test unit\\performance-test-case.lisp"
            ;; Tests
            "Tests\\test-utilities.lisp"
            "Tests\\test-base-model.lisp"
            "Tests\\test-core-objects.lisp"
            "Tests\\test-core-functions.lisp"
            "Tests\\test-properties.lisp"
            "Tests\\test-genetic-operators.lisp"
            "Tests\\test-genetic-operators-cfg.lisp"
            "Tests\\test-image-vector.lisp"
            "Tests\\test-node-selection-functions.lisp"
            "Tests\\test-search-algorithms.lisp"
            "Tests\\test-pattern-matching.lisp"
            "Tests\\test-constant-simplification.lisp"
            "Tests\\test-constant-optimization.lisp"
            "Tests\\test-population.lisp"
            "Tests\\test-dft.lisp"
            "Tests\\test-infix-converison-grammar.lisp"
            "Tests\\test-polynomials.lisp"
            "Tests\\test-lisp-math-expressions.lisp"
            "Tests\\test-population-generators.lisp"
            "Tests\\test-elite-managers.lisp"
            "Tests\\test-source-code-persistence.lisp"
            "Tests\\test-linear-ordering.lisp"
            "Tests\\test-grammar.lisp"
            "Tests\\test-language.lisp"
            "Tests\\test-tree-cfg-language.lisp"
            "Tests\\test-source-code-conversion.lisp"
            "Tests\\test-clark-wright.lisp"
            "Tests\\test-subtree-compression.lisp"
            "Tests\\test-commands.lisp"
            "Tests\\test-events.lisp"
            "Tests\\test-entity-evaluator-x-cl.lisp"
            ;; System performance tests
            "Performance tests\\compression-performance-test.lisp"
            "Performance tests\\constant-optimization-performance-test.lisp"
            "Performance tests\\default-task-global-execution-performance-test.lisp"
            "Performance tests\\default-task-local-execution-performance-test.lisp"
            "Performance tests\\environment-object-transfer-performance-test.lisp"
            "Performance tests\\host-performance-test.lisp"
            "Performance tests\\operator-performance-test.lisp"
            "Performance tests\\source-code-conversion-performance-test.lisp"
            "Performance tests\\task-execution-performance-test.lisp"
            "Performance tests\\instanciation-performance-test.lisp"
            ;; Test suites
            "Tests\\default-test-suites.lisp"
            ;; WORKING STUFF
            "NO-RECURSION.lisp"
            ;; Fixes
            "fixes.lisp"
            "OpenGL\\3d-text-new.lisp")
  :rules 
  ((:in-order-to :compile :all
    (:requires (:load :previous)))))

(setf *system* (compile-system 'gp-tool :force t :load t))

;; #FIX: Patch for search-task-sample-language test
;(initialize-default-search-task-object-templates)

(defun deliver-initialize-system ()
  (initialize-system)
  (initialize-command-line-settings))


