(defpackage "PK.STRINGS"
  (:use "COMMON-LISP")
  (:export "SUBSTITUTE-IN-STRING"
	   "READ-FILE-BY-LINE"))

(in-package "PK.STRINGS")

(defun substitute-in-string (string &rest alist)
  "simple substitution for substrings. the arguments in alist are cons like this (old new)"
  (loop for entry in alist do
       (loop for index = (search (car entry) string :test #'string=) while index do
	    (setf string (concatenate 'string
				      (subseq string 0 index)
				      (second entry); concatenate does not like what cdr returns, e.g. (cdr '(1 2)) = (2)
				      (subseq string (+ index (length (car entry))))))))
  string)

(defmacro read-file-by-line (file l &body b)
  (let ((s (gensym)))
    `(with-open-file (,s (merge-pathnames ,file))
       (do ((,l (read-line ,s) (read-line ,s nil 'eof)))
	   ((eql ,l 'eof))
	 ,@b))))
