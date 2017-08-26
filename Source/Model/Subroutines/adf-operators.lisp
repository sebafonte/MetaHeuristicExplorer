;;; #GUIDELINE

; Koza (1994) defined the architecture of a program to be the total number of trees, the type of each tree (e.g., RPB, ADF, ADI, ADL, ADR, or ADS), the number of arguments (if any) possessed by each tree, and, finally, if there is more than one tree, the nature of the hierarchical references (if any) allowed among the trees (e.g., whether ADF1 can call ADF2).

; There are three ways to determine the architecture of the computer programs that will be evolved:
;
;  1. The user may specify in advance the architecture of the overall program, i.e., perform an architecture-defining preparatory step in addition to the five steps itemised in Chapter  3 .
;
;  2. A run of genetic programming may employ the evolutionary design of the architecture (as described in (Koza,  1994)), thereby enabling the architecture of the overall program to emerge from a competitive process during the run. 
;
;  3. The run may employ a set of architecture-altering operations (Koza, 1994,  1995; Koza, Bennett, Andre, and Keane, 1999) which can create new ADFs, remove ADFs, and increase or decrease the number of inputs an ADF has. Note that many architecture changes (such as those defined in (Koza, 1994)) are designed not to initially change the semantics of the program and, so, the altered program often has exactly the same fitness as its parent. Nevertheless, the new arrangement of ADFs may make it easier for subsequent changes to evolve better programs later



(defclass adf-operator (genetic-operator)
  ((manager :initarg :manager :accessor manager)
   (basic-operator :initarg :basic-operator :accessor basic-operator)))

(defclass rpb-operator (genetic-operator)
  ((manager :initarg :manager :accessor manager)
   (basic-operator :initarg :basic-operator :accessor basic-operator)))


(defmethod operate ((o adf-operator) language expresions)
  (operate-adf o language expresions))

(defmethod operate ((o rpb-operator) language expresions)
  (operate-rpb o language expresions))

(defmethod operate :after ((o adf-operator) language expresions)
  (ensure-not-recurrent (manager o) expresions))

(defun select-rpb (list)
  (car (cdr list)))

(defun select-adf (list)
  (select-random-adf list))

(defun select-random-adf (list)
  (random-element (cdar list)))


;; Helpers for common genetic operators
(defmethod operate-rpb ((o unary-genetic-operator) language expresions)
  (let ((program (select-rpb (first expresions))))
    (operate (basic-operator o) language (list program))))

(defmethod operate-rpb ((o binary-genetic-operator) language expresions)
  (let ((program-a (select-rpb (first expresions)))
        (program-b (select-rpb (second expresions))))
    (operate (basic-operator o) language (list program-a program-b))))

(defmethod operate-adf ((o unary-genetic-operator) language expresions)
  (let ((adf (select-adf program)))
    (operate (basic-operator o) language (list adf))))

(defmethod operate-adf ((o binary-genetic-operator) language expresions)
  (let* ((program-a (program a))
         (program-b (program b))
         (adf-a (select-adf program-a))
         (adf-b (select-adf program-b)))
    (operate (basic-operator o) language (list adf-a adf-b))))


;; Specific ADFs operations should be implemented with classes
;; Koza subrutine creation operator
(defclass adf-creation-genetic-operator (unary-genetic-operator)
  ())

(defmethod operate ((o adf-creation-genetic-operator) language expresions)
  "ADF creation operator like http://www.genetic-programming.com/branch-create2.gif."
  nil)

;; Koza subrutine duplication operator
(defclass adf-subroutine-duplication-genetic-operator (unary-genetic-operator)
  ())

(defmethod operate ((o adf-subroutine-duplication-genetic-operator) language expresions)
  "ADF creation operator like http://www.genetic-programming.com/branch-dup2.gif."
  nil)

;; GLiB-Like subrutine compression operator
(defclass compression-genetic-operator (unary-genetic-operator)
  ())

;; #TODO: Check if necessary
(defclass mutate-arguments-adf-operator (adf-operator)
  ())

(defmethod operate :after ((o mutate-arguments-adf-operator) language expresions)
  (ensure-not-recurrent (manager o) expresions))


;; #TODO: Modify random creation to consider ADFs


;; #NOTE: Not used yet
(defmethod rpb-selector-function (operator a)
  (lambda (list) (list (car (cdr list)))))

(defmethod adf-list-selector-function (operator a)
  'cdar)
