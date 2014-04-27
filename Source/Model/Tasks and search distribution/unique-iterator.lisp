
;;; #VER: Reever, creo que esta obsoleto esto !!!

(defclass unique-iterator (task-iterator)
  ())


(defmethod iterate-task ((iterator unique-iterator) (task search-task))
  t)

(defmethod iterations-count ((iterator unique-iterator))
  1)