
;; Default tree creation function
(defun create-expresion (language tree-max-size max-depth top full)
  "Answer a randomly generated expression for <language> with parameters."
  (let ((max-size (list tree-max-size)))
    (decf (first max-size))
    (create-expresion-recursive language max-depth max-size top full)))

(defun create-expresion-recursive (language max-depth max-size top full)
  "Answer a randomly generated expression for <language> with parameters."
  (cond 
   ;; Check max-depth and max-size
   ((or (<= max-depth 1) (< (first max-size) 1)) 
    (create-terminal language))
   ;; When full creation mode and not exceding max-size, create a function
   ((and (or top full) (<= (1+ (min-language-function-with-args (functions language))) (first max-size)))
    ;; Select a function depending on available arguments and decrement size
    (let* ((function (get-function-with-max-arguments (functions language) (first max-size)))
           (arguments (cadr function)) 
           (function (car function)))
      (decf (first max-size) arguments)
      (cons function (create-function-arguments language arguments (1- max-depth) max-size top full))))
   ;; 
   (t (if (zerop (random 2))
          ;; Function
          (let* ((function (get-function-with-max-arguments (functions language) (first max-size)))
                 (arguments (cadr function))
                 (function (car function)))
            (decf (first max-size) arguments)
            (cons function (create-function-arguments language arguments (1- max-depth) max-size top full)))
        ;; Terminal
        (create-terminal language)))))

(defun create-function-arguments (language arguments max-depth max-size top full)
  "Answer a function for an expression in <language>."
  (if (= arguments 0)
      nil
    (cons (create-expresion-recursive language max-depth max-size nil full) 
          (create-function-arguments language (1- arguments) max-depth max-size nil full))))

(defun create-terminal (language)
  "Answer a terminal element for an expression in <language>."
  (let ((terminal (random-element (terminals language))))
    (if (eq terminal :constant)
        (create-constant (constants-strategy language))
      terminal)))


#|
;; Full function with no size control
(defun create-expresion-full (language max-depth)
  "Answer a randomly generated expression for <language> with parameters."
  (if (<= max-depth 1) 
      (create-terminal language)
    (let* ((function (get-function-with-max-arguments-default (functions language)))
           (arguments (cadr function)) 
           (function (car function)))
      (cons function (create-function-arguments language arguments (1- max-depth) full)))))

;; Grow function with no size control
(defun create-expresion-grow (language max-depth)
  "Answer a randomly generated expression for <language> with parameters."
  (if (or (<= max-depth 1) (< (random-real 0 1) 0.5))
      (create-terminal language)
    (let* ((function (get-function-with-max-arguments-default (functions language)))
           (arguments (cadr function))
           (function (car function)))
      (cons function (create-function-arguments language arguments (1- max-depth) grow)))))


(defun get-function-with-max-arguments-default (list max-arguments)
  "Answer a random function description <list> which <max-arguments> as a constraint."
  (let ((new-list))
    (dolist (i list)
      (appendf new-list (list i)))
    (nth (random (list-length new-list)) new-list)))
|#