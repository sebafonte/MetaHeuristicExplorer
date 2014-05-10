
(defun initialize-genetic-operators ()
  (system-add
   ;; Binary operators
   (make-instance 'one-point-array-crossover
                  :name 'one-point-array-crossover)
   (make-instance 'one-point-array-mutation
                  :name 'one-point-array-mutation)
   ;;; TREE Operators
   ;; Crossover
   (make-instance 'subtree-crossover
                  :name 'crossover
                  :value-function #'crossover
                  :min-subtree-depth 1)
   (make-instance 'subtree-crossover
                  :name 'crossover-koza
                  :value-function #'crossover-koza
                  :min-subtree-depth 2)
   (make-instance 'subtree-crossover
                  :name 'crossover-cfg
                  :value-function #'crossover-cfg
                  :source-selection-function 'crossover-cfg-source-selection
                  :target-selection-function 'crossover-cfg-target-selection-weight-depth
                  :min-subtree-depth nil)
   ;; Random creation
   (make-instance 'cfg-mutation
                  :name 'random-create-cfg
                  :value-function 'random-create-cfg
                  :source-selection-function nil
                  :target-selection-function nil
                  :production-selection-weight-function 'lambda-weight-equal-random-selection-list
                  :min-subtree-depth nil)
   (make-instance 'cfg-mutation
                  :name 'initial-random-create-cfg-1
                  :value-function 'random-create-cfg-initial-size
                  :source-selection-function nil
                  :target-selection-function nil
                  :production-selection-weight-function 'lambda-weight-equal-random-selection-list
                  :min-subtree-depth nil)
   (make-instance 'cfg-mutation
                  :name 'initial-random-create-cfg-2
                  :value-function 'random-create-cfg-size
                  :source-selection-function nil
                  :target-selection-function nil
                  :production-selection-weight-function 'lambda-weight-equal-random-selection-list
                  :min-subtree-depth nil)
   ;; Mutation
   (make-instance 'mutation
                  :name 'mutate
                  :value-function #'mutate
                  :source-selection-function 'lambda-mutate-selection-function)
   (make-instance 'mutation
                  :name 'mutate-koza
                  :value-function #'mutate-koza)
   (make-instance 'point-mutation
                  :name 'mutate-point
                  :value-function 'mutate-point
                  :source-selection-function 'lambda-point-mutation-selection-function)
   (make-instance 'cfg-mutation
                  :name 'mutate-cfg
                  :value-function 'mutate-cfg
                  :source-selection-function 'crossover-cfg-source-selection
                  :target-selection-function 'crossover-cfg-target-selection-weight-depth
                  :production-selection-weight-function 'lambda-weight-heuristic-1-random-selection-list)
   (make-instance 'cfg-mutation
                  :name 'mutate-reuse-cfg
                  :value-function 'mutate-reuse-cfg
                  :source-selection-function 'mutate-production-cfg-source-selection
                  :target-selection-function nil
                  :production-selection-weight-function 'lambda-weight-equal-random-selection-list)
   (make-instance 'cfg-mutation
                  :name 'mutate-production-cfg
                  :value-function 'mutate-production-cfg
                  :source-selection-function 'mutate-production-cfg-source-selection
                  :target-selection-function nil
                  :production-selection-weight-function 'lambda-weight-equal-random-selection-list)
   ;; Simplification
   (make-instance 'branch-delete
                  :name 'branch-delete)
   (make-instance 'subtree-crossover
                  :name 'branch-delete-cfg
                  :value-function #'mutate-branch-delete-cfg
                  :source-selection-function 'branch-delete-cfg-source-selection
                  :target-selection-function nil))
  (system-add
   ;; ADF tree operators
   ;;; #VER
   (make-instance 'rpb-operator
                  :name 'rpb-crossover-koza
                  :basic-operator (system-get 'crossover-koza))
   (make-instance 'adf-operator
                  :name 'adf-crossover-koza
                  :basic-operator (system-get 'crossover-koza))
   (make-instance 'rpb-operator
                  :name 'rpb-branch-delete
                  :basic-operator (system-get 'mutate-koza))
   (make-instance 'adf-operator
                  :name 'adf-branch-delete
                  :basic-operator (system-get 'mutate-koza))
   (make-instance 'rpb-operator
                  :name 'rpb-mutate
                  :basic-operator (system-get 'branch-delete))
   (make-instance 'adf-operator
                  :name 'adf-mutate
                  :basic-operator (system-get 'branch-delete))
   (make-instance 'adf-operator
                  :name 'adf-mutate-arguments
                  :basic-operator (system-get 'mutate-point))
   ;; Compression tree operators
   (make-instance 'subroutine-compression-operator
                  :name 'compress-1
                  :value-function 'compress-1)
   (make-instance 'subroutine-depth-compression-operator
                  :name 'compress-2
                  :value-function 'compress-2)
   (make-instance 'subroutine-compression-operator
                  :name 'expand-1
                  :value-function 'expand-1)
   (make-instance 'subroutine-compression-operator
                  :name 'expand-all
                  :value-function 'expand-all)
   (make-instance 'subroutine-compression-operator
                  :name 'shake-1
                  :value-function 'shake-1))
  ;;; VRP Genetic operators
  (system-add
   (make-instance 'sample-vrp-mutation
                  :name 'vrp-split-tour-mutate
                  :value-function 'sample-vrp-split-tour-mutate)
   (make-instance 'sample-vrp-mutation
                  :name 'vrp-merge-tour-mutate
                  :value-function 'sample-vrp-merge-tour-mutate)
   (make-instance 'sample-vrp-mutation
                  :name 'vrp-city-in-tour-mutate
                  :value-function 'sample-vrp-city-in-tour-mutate)
   (make-instance 'sample-vrp-mutation
                  :name 'vrp-city-inter-tour-mutate
                  :value-function 'sample-vrp-city-inter-tour-mutate)
   (make-instance 'sample-vrp-crossover
                  :name 'vrp-subtour-crossover
                  :value-function 'sample-vrp-subtour-crossover)
   ;;; LOP LIST operators
   (make-instance 'lop-mutation
                  :name 'permutate-random-row
                  :value-function 'permutate-random-row)
   (make-instance 'lop-mutation
                  :name 'permutate-random-column
                  :value-function 'permutate-random-column)
   (make-instance 'lop-mutation
                  :name 'permutate-best-row
                  :value-function 'permutate-best-row)
   (make-instance 'lop-mutation
                  :name 'permutate-best-column
                  :value-function 'permutate-best-column)
   (make-instance 'lop-mutation
                  :name 'permutate-next-row
                  :value-function 'permutate-next-row)
   (make-instance 'lop-mutation
                  :name 'permutate-next-column
                  :value-function 'permutate-next-column)
   (make-instance 'lop-mutation
                  :name 'lop-simplification-1
                  :value-function 'simplification-1)
   (make-instance 'lop-crossover
                  :name 'lop-crossover-1
                  :copy-length 1
                  :value-function 'lop-crossover-1)
   (make-instance 'lop-crossover
                  :name 'lop-crossover-2
                  :copy-length 2
                  :value-function 'lop-crossover-2)
   (make-instance 'lop-crossover
                  :name 'lop-crossover-3
                  :copy-length 3
                  :value-function 'lop-crossover-3)))
  

(defclass selection-tag (base-model)
  ((subject :initarg :subject :accessor subject)
   (tag :initarg :tag :accessor tag)
   (value :initarg :value :accessor value)))


(defun default-genetic-operators-probability-lisp-expression ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'crossover)                  0.0)
        (list (system-get 'crossover-koza)             0.3)
        (list (system-get 'crossover-cfg)              0.2)
        (list (system-get 'branch-delete)              0.2)
        (list (system-get 'branch-delete-cfg)          0.0)
        (list (system-get 'mutate)                     0.0)
        (list (system-get 'mutate-point)               0.2)
        (list (system-get 'mutate-koza)                0)
        (list (system-get 'mutate-cfg)                 0.1)
        (list (system-get 'mutate-reuse-cfg)           0.0)
        (list (system-get 'mutate-production-cfg)      0.0)
        (list (system-get 'random-create-cfg)          0.0)))

(defun default-genetic-operators-probability-polynomial-expression ()
  "Answer a structure with default operations with each normalized probability."
  (list 
   (list (system-get 'crossover-cfg)                   0.5)
   (list (system-get 'mutate-cfg)                      0.5)
   (list (system-get 'mutate-reuse-cfg)                0.0)
   (list (system-get 'mutate-production-cfg)           0.0)
   (list (system-get 'random-create-cfg)               0.0)
   (list (system-get 'branch-delete-cfg)               0.0)))

(defun default-genetic-operators-probability-linear-ordering ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'permutate-random-row)       0.0)
        (list (system-get 'permutate-random-column)    0.0)
        (list (system-get 'permutate-best-row)         0.5)
        (list (system-get 'permutate-best-column)      0.5)
        (list (system-get 'permutate-next-row)         0.0)
        (list (system-get 'permutate-next-column)      0.0)
        (list (system-get 'lop-crossover-1)            0.0)
        (list (system-get 'lop-crossover-2)            0.0)
        (list (system-get 'lop-crossover-3)            0.0)
        (list (system-get 'lop-simplification-1)       0.0)))

(defun default-genetic-operators-probability-sample-vrp ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'vrp-split-tour-mutate)      0.25)
        (list (system-get 'vrp-merge-tour-mutate)      0.25)
        (list (system-get 'vrp-city-in-tour-mutate)    0.25)
        (list (system-get 'vrp-city-inter-tour-mutate) 0.0)
        (list (system-get 'vrp-subtour-crossover)      0.25)))

(defun default-genetic-operators-probability-search-task ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'crossover-cfg)              0.2)
        (list (system-get 'mutate-cfg)                 0.2)
        (list (system-get 'mutate-reuse-cfg)           0.2)
        (list (system-get 'mutate-production-cfg)      0.2)
        (list (system-get 'branch-delete-cfg)          0.2)
        (list (system-get 'mutate-production-cfg)      0.0)))

(defun default-genetic-operators-probability-adf-lisp-expression ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'rpb-crossover-koza)         0.15)
        (list (system-get 'adf-crossover-koza)         0.15)
        (list (system-get 'adf-mutate)                 0.15)
        (list (system-get 'rpb-mutate)                 0.15)
        (list (system-get 'adf-branch-delete)          0.15)
        (list (system-get 'rpb-branch-delete)          0.25)
        (list (system-get 'adf-mutate-arguments)       0.0)))

(defun default-genetic-operators-probability-compression-lisp-expression ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'crossover-koza)             0.25)
        (list (system-get 'crossover-cfg)              0.25)
        (list (system-get 'branch-delete)              0.0)
        (list (system-get 'mutate-point)               0.0)
        (list (system-get 'mutate-cfg)                 0.1)
        (list (system-get 'random-create-cfg)          0.1)
        (list (system-get 'compress-1)                 0.1)
        (list (system-get 'compress-2)                 0.1)
        (list (system-get 'expand-1)                   0.05)
        (list (system-get 'expand-all)                 0.05)
        (list (system-get 'shake-1)                    0.0)))

(defun default-genetic-operators-probability-texture-separate ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'crossover-cfg)              0.45)
        (list (system-get 'mutate-cfg)                 0.45)
        (list (system-get 'random-create-cfg)          0.1)
        (list (system-get 'shake-1)                    0.0)))

(defun default-genetic-operators-probability-texture-enclosure ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'crossover-cfg)              0.45)
        (list (system-get 'mutate-cfg)                 0.45)
        (list (system-get 'random-create-cfg)          0.1)
        (list (system-get 'shake-1)                    0.0)))

(defun default-genetic-operators-probability-binary-ga ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'one-point-array-crossover)  0.9)
        (list (system-get 'one-point-array-mutation)   0.1)))