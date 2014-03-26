(defun foobar ()
  (multiple-value-bind (a b) (floor 5.9) (list a b)))

(defmacro curry (f val &optional (fnam nil))
  (if fnam
      `(defun ,fnam (&rest x) (apply ,f ,val x))
      `(lambda (&rest x) (apply ,f ,val x))))

(mapcar (curry #'* 2) '(1 2 3 4))

(progn (princ "am i seen?") (finish-output))

(typep 3 '(and fixnum (satisfies evenp)))

(reduce #'+ '((a 1) (b 2) (c 3) (d 4) (e 5)) :key #'cadr)

(mapcar (lambda (x) (cons x (length x))) '("a" "ab" "abc"))

(reduce #'max '(1 2 3 5 4 6))

(let ((tree (list)))
  (push (cons :a 1) tree))

(defmacro compose (f1 f2)
  `(lambda (x) (funcall ,f1 (funcall ,f2 x))))

(funcall (compose (curry #'+ 3) (curry #'* 2)) 3)

(destructuring-bind (a &rest b) (list 1 2 3 4) (princ b))

(defun take (list n) 
  (subseq list 0 n))

(defun drop (list n) 
  (subseq list n))

(defun take-right (list n) 
  (subseq list (- (length list) n)))

(defun drop-right (list n) 
  (subseq list 0 (- (length list) n)))

(defun split-at (list n) (values (take list n) (drop list n)))

(defmacro with-collectors (collectors &body rest)
  (let ((gsyms (mapcar (lambda (_)
			 (declare (ignore _))
			 (gensym)) collectors)))
    `(let ,gsyms
       (flet ,(mapcar (lambda (c g) 
		       `(,c (&rest x) 
			    (if x (push (car x) ,g) (reverse ,g)))) collectors gsyms)
	 (progn ,@rest (values ,@gsyms))))))

(defmacro named-let (name arguments &body body)
  "Create a value function named name that operates on a list of (arg initform) pairs"
  (multiple-value-bind (args initforms) (split-args-and-init arguments)
  `(labels ((,name (,@args)
	      (declare (optimize (debug 0) (safety 0) (speed 3)))
	      ,@body)) 
     (,name ,@initforms))))

(named-let foo ((a 1) (b 0)) 
  (if (= b 10) 
      a 
      (foo (+ a 20) (1+ b))))

(defun split-args-and-init (list)
  (loop for (arg init) in list 
     collect arg into arglist 
     collect init into initlist 
     finally (return (values arglist initlist))))
