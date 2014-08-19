(defun select-by-string (s)
(string-case s ("foobar" (print "foobar")) ("deadbeef" (print "deadbeef")) (t (print "none"))))

(defun generic-in-listp (test object list)
  "O(length(list))"
  (reduce #'(lambda (x y) (or x y)) (mapcar #'(lambda (x) (funcall test object x)) list)))

(defun string-in-listp (str list)
 (generic-in-listp #'string= str list))

(defun split-car (list)
  "split the car from the rest of the list"
 (values (car list) (cdr list)))

(defgeneric in-listp (object list)
 (:method ((object string) (list list)) (funcall #'generic-in-listp #'string= object list))
 (:method ((object number) (list list)) (funcall #'generic-in-listp #'= object list)))

(defmacro string-case (switch &rest cases)
  "simple case expression for strings"
  (if (not (= (length cases) 0))
      (multiple-value-bind (first-case the-rest) (split-car cases)
	(declare (inline split-car))
	`(flet ((string-case-compare (str o)
		  (if (listp o) (in-listp str o) (string= str o))))
	   (if (string-case-compare ,switch ,(car first-case)) (progn ,@(cdr first-case)) (string-case ,switch ,@the-rest))))
      'nil))
       