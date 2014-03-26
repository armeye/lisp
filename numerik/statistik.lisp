(defun arith-mittel (x)
  (/ (loop for i in x sum i) (length x)))

(defun arith-mittel-mr (l)
  (/ (reduce #'+ l) (length l)))

(defun probabilities-sanep (x)
  (if (= (loop for i in x sum i) 1) t nil))

(defun erwartungswert (x)
  (loop for (probability value) in x sum (* probability value)))

(defgeneric varianz (values e))

(defmethod varianz ((values list) (e number))
  (loop for i in values sum (expt (- i e) 2)))

(defmethod varianz ((values list) (e list))
  (varianz values (erwartungswert e)))
