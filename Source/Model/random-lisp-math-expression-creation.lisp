
(defun create-expresion (language tree-max-size max-depth top full)
  "Answer a randomly generated expression for <language> with parameters."
  (declare (special max-size))
  (let ((max-size tree-max-size))
    (declare (special max-size))
    (decf max-size)
    (create-expresion-recursive language max-depth top full)))

(defun create-expresion-recursive (language max-depth top full)
  "Answer a randomly generated expression for <language> with parameters."
  (declare (special max-size))
  (cond 
   ;; If we are at the end (max-size = 1) create a terminal
   ((or (<= max-depth 1) (<= max-size 1)) 
    (create-terminal language))
   ;; full creation mode? If so, create a function
   ((or top full)
    ;; Select a function depending on available arguments 
    (let* ((function (get-function-with-max-arguments (functions language) max-size))
           (arguments (cadr function)) 
           (function (car function)))
      ;; Decrement max-size because we created a function node
      (decf max-size arguments)
      (cons function (create-function-arguments language arguments (- max-depth 1) full))))
   ;; Select : Function or terminal?
   (t (let ((type (case (random 2) (0 'TERMINAL) (1 'FUNCTION))))
        ;; When creating a function
        (if (equal type 'FUNCTION)
            ;; #TODO: Select from (functions language) checking max-size
            (let* ((function (random-element (functions language)))
                   (arguments (cadr function))
                   (function (car function)))
              (decf max-size arguments)
              (cons function (create-function-arguments language arguments (- max-depth 1) full)))
          ;; When creating a terminal: 'CONSTANT or 'VARIABLE
          (create-terminal language))))))

(defun create-function-arguments (language arguments max-depth full)
  "Answer a function for an expression in <language>."
  (if (= arguments 0)
      nil
    (cons (create-expresion-recursive language max-depth nil full) 
          (create-function-arguments language (- arguments 1) max-depth full))))

(defun create-terminal (language)
  "Answer a terminal element for an expression in <language>."
  (let ((terminal (random-element (terminals language))))
    (if (eq terminal :constant)
        (create-constant (constants-strategy language))
      terminal)))
