(defclass test-pattern-matching (test-base-model) ())


(defmethod test-pattern-matching-examples-1 ((o test-pattern-matching))
  "Verifies whether a algorithm reinitialize dependent properties for <o>."
  (check (match '(p a b c a) '(p ?x ?y c ?x)))
  (check (match '(p ?x b ?y a) '(p ?y b c a)))
  (check (null (match '(a b c) '(a a a)))))
