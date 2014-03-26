(defpackage :pk.numerik (:use :common-lisp))

(in-package :pk.numerik)

(declaim (optimize (speed 3)))

(defun simpson (f a b)
  (declare (double-float a b)
	   ((function (double-float) double-float) f))
     (let ((c (/ (+ a b) 2.0d0))
	   (h3 (/ (abs (- b a)) 6.0d0)))
       (declare (type double-float c h3))
       (* h3 (+ (funcall f a) (* (funcall f c) 4.0d0)) (funcall f b))))

(defun recursive-adaptive-simpson (f a b eps sum)
  (declare (double-float a b eps sum)
	   ((function (double-float) double-float) f))
     (let* ((c (/ (+ a b) 2.0d0))
	   (left (simpson f a c))
	   (right (simpson f c b)))
       (declare (type double-float c left right))
       (if (<= (abs (- (+ left right) sum)) (* 15.0d0 eps))
	   (+ left right (/ (- (+ left right) sum) 15.0d0))
	 (+ (recursive-adaptive-simpson f a c (/ eps 2.0d0) left) (recursive-adaptive-simpson f c b (/ eps 2.0d0) right)))))

(defun adaptive-simpson (f a b eps)
  (declare (double-float a b eps))
     (recursive-adaptive-simpson f a b eps (simpson f a b)))

;;(print (time (adaptive-simpson #'(lambda (x) (/ 1.0d0 (sqrt (- 1.0d0 (* x x))))) -1.0d0 1.0d0 0.00000001d0)))

(defun horner (x a)
  "Horner schema fuer Polynomberechnung"
  (declare (number x))
  (loop for i number in (cdr a) with s number = (car a) do (setf s (+ (* x s) i))  finally (return s)))