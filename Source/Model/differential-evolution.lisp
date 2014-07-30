
;; Special differential evolution operators
(defclass de-crossover (binary-genetic-operator)
  ((parameter :initarg :parameter :accessor parameter)))


   def bin_crossover(self, oldgene, newgene):
        new = oldgene[:]
        for i in xrange(len(oldgene)):
            if random.random() < self.crossover_rate:
                new[i] = newgene[i]
        return new


(defclass de-mutation (unary-genetic-operator)
  ((scaling :initarg :scaling :accessor scaling)))


;; Random initializer: random creation population could delegate here
(defclass de-random-initializer ()
  ())

(defmethod generate-individual ()
  nil)

(defmethod generate-population ()
  nil)


;; Algorithm
(defclass differential-evolution (evolutionary-algorithm)
  nil)

(defmethod initialize-properties :after ((a evolutionary-algorithm))
  "Initialize <a> properties."
  (add-properties-from-values
   a
   (:name 'eps :label "Eps convergence" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 2 :editor 'integer-editor)
   (:name 'eps :label "Difference method" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 2 :editor 'integer-editor)))

(defmethod search-loop ((a differential-evolution))
  nil)

#|


  def solve(self, newgens=100):
        """Run for newgens more generations.

        Return best parameter vector from the entire run.
        """
        for gen in xrange(self.generations+1, self.generations+newgens+1):
            for candidate in range(self.npop):
                trial = self.get_trial(candidate)
                trial_value = self.func(trial, *self.args)
                if trial_value < self.pop_values[candidate]:
                    self.population[candidate] = trial
                    self.pop_values[candidate] = trial_value
                    if trial_value < self.best_value:
                        self.best_vector = trial
                        self.best_value = trial_value
            self.best_val_history.append(self.best_value)
            self.best_vec_history.append(self.best_vector)
            if self.converged():
                break
        self.generations = gen
        return self.best_vector

(defmethod diff-1 ()
  nil)

(defmethod diff-2 ()
  nil)



def diff1(self, candidate):
        i1, i2 = self.select_samples(candidate, 2)
        y = [(self.population[i1][c] - self.population[i2][c]) for c in xrange(self.ndim)]
        y = [self.scale*i for i in y]
        return y

def diff2(self, candidate):
        i1, i2, i3, i4 = self.select_samples(candidate, 4)
        y = ([(self.population[i1][c] - self.population[i2][c]+self.population[i3][c] - self.population[i4][c]) for c in xrange(self.ndim)])
        y = [self.scale*i for i in y]
        return y

def choose_rand_to_best(self, candidate):
        return ((1-self.scale) * self.population[candidate] + self.scale * self.best_vector)


(defmethod mirror-bounds (algorithm o)
  (dotimes (i n)
    (if (< (aref o i) lbound)
        (progn 
          (setf (aref o i) (- (* 2 lbound) (aref o i)))
          (if (< (aref o i) lbound)
              (setf (aref o i) (random-real lbound ubound))))
      (when (> value ubound)
        (setf (aref o i) (- (* 2 ubound) (aref o i)))
        (if (< (aref o i) lbound)
            (setf (aref o i) (random-real lbound ubound)))))))

(defmethod hug-boungs ()
  nil)


|#