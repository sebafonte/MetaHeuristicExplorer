
(defun initialize-default-fitness-evaluators ()
  (system-add
   ;; Evaluators for entity-function-x
   (make-instance 'entity-function-x-evaluator
                  :name 'fine-0-10-function-x-evaluator
                  :description "Fine 0-10"
                  :samples 250
                  :measure-start 0
                  :measure-end 10)
   (make-instance 'entity-function-x-evaluator
                  :name 'medium-0-10-function-x-evaluator
                  :description "Medium 0-10"
                  :samples 100
                  :measure-start 0
                  :measure-end 10)
   (make-instance 'entity-function-x-evaluator
                  :name 'gross-0-10-function-x-evaluator
                  :description "Gross 0-10"
                  :samples 20
                  :measure-start 0
                  :measure-end 10)
   ;; Evaluators for entity-function-x with OpenCL
   (make-instance 'entity-function-x-cl-evaluator
                  :name 'fine-0-10-function-x-cl-evaluator
                  :description "Fine 0-10 CL"
                  :samples 250
                  :measure-start 0
                  :measure-end 10)
   (make-instance 'entity-function-x-cl-evaluator
                  :name 'medium-0-10-function-x-cl-evaluator
                  :description "Medium 0-10 CL"
                  :samples 100
                  :measure-start 0
                  :measure-end 10)
   (make-instance 'entity-function-x-cl-evaluator
                  :name 'gross-0-10-function-x-cl-evaluator
                  :description "Gross 0-10 CL"
                  :samples 20
                  :measure-start 0
                  :measure-end 10)
   ;; Evlauators for entity-function-x with sample values
   (make-instance 'entity-function-x-values-evaluator
                  :name 'sample-values-function-x-evaluator
                  :description "Samples X"
                  :samples '((1 1) (2 2) (3 3) (4 4)))
   ;; Evlauators for entity-function-x-y with sample values
   (make-instance 'entity-function-xy-values-evaluator
                  :name 'sample-values-function-xy-evaluator
                  :description "Samples XY"
                  :samples '((1 1 1) (2 2 2) (3 3 3) (4 4 4)))
   ;; Evaluators for entity-function-x (sucessions)
   (make-instance 'entity-sucession-evaluator
                  :name 'sucession-evaluator
                  :description "Default sucession evaluator"
                  :value-list '((1 1) (2 2) (3 6) (4 42) (5 1806)))
   ;; Evaluators for entity-function-x-y
   (make-instance 'entity-function-x-y-evaluator
                  :name 'fine-0-10-function-x-y-evaluator
                  :description "Fine 0-10"
                  :samples 50
                  :measure-start 0
                  :measure-end 10)
   (make-instance 'entity-function-x-y-evaluator
                  :name 'medium-0-10-function-x-y-evaluator
                  :description "Medium 0-10"
                  :samples 30
                  :measure-start 0
                  :measure-end 10)
   (make-instance 'entity-function-x-y-evaluator
                  :name 'gross-0-10-function-x-y-evaluator
                  :description "Gross 0-10"
                  :samples 10
                  :measure-start 0
                  :measure-end 10)
   (make-instance 'entity-function-x-y-evaluator
                  :name 'very-gross-0-10-function-x-y-evaluator
                  :description "Very gross 0-10"
                  :samples 5
                  :measure-start 0
                  :measure-end 10)
   (make-instance 'entity-function-x-y-ponderated-evaluator
                  :name 'gross-pond-0-10-function-x-y-evaluator
                  :description "Gross ponderated 0-10"
                  :samples 5
                  :measure-start 0
                  :measure-end 10
                  :ponderation 1)
   ;; Evaluators for entity-image-rgb
   (make-instance 'entity-image-rgb-evaluator
                  :name 'entity-rgb-evaluator
                  :description "Anti constants"
                  :fitness-function 'evaluate-no-evaluation)
   ;; Evaluators for seamless image objects 
   (make-instance 'entity-image-seamless-evaluator
                  :name 'entity-seamless-basic
                  :description "Seamless basic"
                  :fitness-function 'evaluar-imagen-seamless)
   ;; Evaluators for entity-linear-ordering
   (make-instance 'entity-linear-ordering-evaluator
                  :name 'entity-linear-ordering-evaluator
                  :description "LOP evaluator")
   ;; Evaluators for VRP sample problem
   (make-instance 'entity-sample-vrp-evaluator
                  :name 'sample-vrp-evaluator
                  :description "VRP sample evaluator")
   (make-instance 'entity-vrp-evaluator
                  :name 'default-vrp-evaluator
                  :description "VRP default evaluator")
   (make-instance 'entity-dvrp-evaluator
                  :name 'default-dvrp-evaluator
                  :description "DVRP default evaluator"))
  (system-add
   ;; Evaluators for search tasks
   (make-instance 'search-algorithm-objetive-fitness-evaluator
                  :name 'default-composite-algorithm-evaluator-1
                  :description "Algorithm fitness evaluator 1")
   ;; Evaluators for search tasks
   (make-instance 'search-task-objetive-fitness-evaluator
                  :name 'default-search-task-objetive-fitness-evaluator
                  :description "Objetive fitness evaluator")))

