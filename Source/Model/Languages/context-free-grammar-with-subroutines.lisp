
(defclass context-free-grammar-with-subroutines (context-free-grammar)
  ((language :initarg :language :initform nil :accessor language)))


(defmethod arity-token ((g context-free-grammar-with-subroutines) word)
  (arity-token (language g) word))