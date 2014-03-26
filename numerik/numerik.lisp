(declaim (optimize speed))

(defun vector-sum (v)
  "Summe der einzelnen Vektorkomponenten"
  (loop for i number in v sum i))

(defun vector-length (v)
  "Betrag eines Vektors"
  (sqrt (loop for i number in v sum (expt i 2))))

(defun vmul-skalar (a vs)
  "Multiplikation des vektors vs mit dem skalar a"
  (loop for v number in vs collect (* a v)))

(defun vadd-binary (v1 v2)
  "Hilfsfunktion fur vadd"
  (loop for x number in v1 for y number in v2 collect (+ x y)))

(defun vadd (vs)
  "Addition einer beliebigen Anzahl von Vektoren"
  (reduce #'vadd-binary vs))

(defun linkomb (&rest av)
  "Linearkombination"
  (vadd (loop for (a v) in av collect (vmul-skalar a v))))

(defun arith-mittel(&rest v)
  (/ (loop for i in v sum i) (length v)))

(defun bisektion (f a b &optional (eps (coerce 1.0e-5 'double-float)))
  "Nullstellen ueber das Bisektionsverfahren"
  (declare (double-float a b eps)
	   (ftype (function (double-float double-float) double-float) f))
  (let* ((aa (funcall f a)) 
	 (bb (funcall f b)) 
	 (c (/ (+ a b) 2.0d0))
	 (cc (funcall f c)))
    (declare (double-float aa bb c cc))
    (cond ((< (abs aa) eps) a)
	  ((< (abs bb) eps) b)
	  ((< (abs cc) eps) c)
	  ((/= (signum aa) (signum cc)) (bisektion f a c eps))
	  ((/= (signum cc) (signum bb)) (bisektion f c b eps))
	  (t nil))))

(defun lucas-lehmer-primep (p)
  "Testet eine Integer ob es sich um eine Mersenne Primzahl handelt. Die Zahl p muss zwischen 0 und intmax liegen.
  http://en.wikipedia.org/wiki/Lucas-Lehmer_primality_test
  http://en.wikipedia.org/wiki/Mersenne_Prime"
  (declare ((integer 0 *) p))
  (let ((s 4)
	(m (- (expt 2 p) 1)))
    (declare ((integer 0 *) s m))
    ;        var  max    finally
    (dotimes (i (- p 2) (if (= s 0) t nil))
      (declare (ignore i))
      ; body
      (setf s (mod (- (expt s 2) 2) m)))))

(defun fermat-primep (p &optional (k 10))
  (declare ((number 0 *) p))
  (loop named floop repeat k do 
       (let ((a (random (1- p))))
	 (when (= (mod (expt a (1- p)) p) 1) (return-from floop t)))))


(defun polval (x alist)
  "compute the polynimial value of x by the coefficiants in alist. this can handle complex numbers"
  (declare (number x))
  (loop for a number in (nreverse alist) 
     for i fixnum = 0 then (1+ i) 
     sum (* (expt x i) a)))

(defun pol-zerop (x alist)
  "testet ob x eine Nullstelle des polynoms mit den koeffizienten in alist ist"
  (if (= (polval x alist) 0) t nil))
