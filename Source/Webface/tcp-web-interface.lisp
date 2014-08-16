(defclass tcp-web-inferface ()
  ())


(defmethod object-description-properties (entity)
  '((objetive-class 
     seed
     algorithm
     (algorithm population-size)
     (algorithm generations)
     (algorithm initialization-method)
     (builder runs)
     (language)
     (language min-size)
     (language max-size))))



