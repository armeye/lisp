(defun polval (x alist)
  "compute the polynimial value of x by the coefficiants in alist. this can handle complex numbers"
  (declare (number x))
  (loop for a number in (nreverse alist) 
     for i fixnum = 0 then (1+ i) 
     sum (* (expt x i) a)))