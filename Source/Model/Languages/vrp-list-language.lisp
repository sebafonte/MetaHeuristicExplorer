(defclass vrp-list-language (language)
  ())


(defmethod max-size-for ((l vrp-list-language) evaluator)
  (max-vehicles evaluator))

