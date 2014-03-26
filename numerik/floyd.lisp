(defpackage :pk.algodat 
  (:use :common-lisp :common-lisp-user)
  (:export :floyd :+infinity+))

(in-package :pk.algodat)

(defconstant +infinity+ most-positive-fixnum "Beschreibt wege die nicht zu erreichen sind")
(defvar *adj* (make-array '(10 10) :element-type 'fixnum 
			  :initial-contents (let ((i +infinity+))
							 `((0 1 ,i ,i 6 7 ,i ,i ,i 8) ; a
							   (1 0 ,i ,i 3 ,i 2 ,i ,i 4) ; b
							   (,i ,i 0 9 ,i ,i ,i 3 5 ,i) ; c
							   (,i ,i 7 0 ,i ,i ,i 6 3 ,i) ; d
							   (7 ,i ,i ,i 0 1 6 ,i ,i ,i) ; e
							   (,i 4 ,i ,i 2 0 ,i ,i ,i 3) ; f
							   (8 ,i ,i ,i 5 2 0 ,i ,i ,i) ; g
							   (,i ,i 1 4 ,i ,i ,i 0 1 ,i) ;h
							   (,i ,i 2 ,i ,i ,i ,i 3 0 ,i) ; i
							   (,i 3 ,i ,i 4 ,i 1 ,i ,i 0) ; j
							   ))))

(defun square-matrixp (array)
  (let ((dimensions (array-dimensions array)))
    (if (and (= (length dimensions) 2) (= (first dimensions) (second dimensions))) (values t (car dimensions)) (values nil (car dimensions)))))

(defun floyd (matrix)
  "floyds algorithmus fuer die kuerzesten wege in einem graphen"
  (declare (optimize speed)
	   (type (array fixnum *) matrix))
  (multiple-value-bind (square? dimension) (square-matrixp matrix) 
    (if (eql square? t) 
	(dotimes (k dimension matrix)
	  (dotimes (i dimension)
	    (dotimes (j dimension)
	      (let ((d (aref matrix i j)))
		(setf (aref matrix i j) 
		      (if (or (= (aref matrix i k) +infinity+)
			      (= (aref matrix k j) +infinity+)) 
			  d
			  (min d (+ (aref matrix i k) (aref matrix k j)))))))))
	(error "keine quadratische matrix"))))

(defun test-floyd ()
  (if (equalp (floyd *adj*) #2A((0 1 536870911 536870911 4 5 3 536870911 536870911 5)
				(1 0 536870911 536870911 3 4 2 536870911 536870911 4)
				(536870911 536870911 0 7 536870911 536870911 536870911 3 4 536870911)
				(536870911 536870911 5 0 536870911 536870911 536870911 6 3 536870911)
				(6 5 536870911 536870911 0 1 5 536870911 536870911 4)
				(5 4 536870911 536870911 2 0 4 536870911 536870911 3)
				(7 6 536870911 536870911 4 2 0 536870911 536870911 5)
				(536870911 536870911 1 4 536870911 536870911 536870911 0 1 536870911)
				(536870911 536870911 2 7 536870911 536870911 536870911 3 0 536870911)
				(4 3 536870911 536870911 4 3 1 536870911 536870911 0))) 
      (print "floyd test passed") 
      (error "floyd test failed")))