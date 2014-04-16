(defconstant +placeholder+ '<>)

(defmacro cut (f &rest args)
  "curry on steroids. you can curry arbitrary arguments of a function by substituting them with <>"
 (multiple-value-bind (pargs largs) (cut-helper args)
   `(lambda (,@largs) (funcall ,f ,@pargs))))

(defun cut-helper (args)
  "Helper function for cut. process a list args into args for the function and for the lambda. This will not be exported"
  (let ((args-processed (list))
	(args-lambda (list)))
    (loop for i in args do (if (equalp i +placeholder+)
			       (let ((sym (gensym "CUT")))
				 (progn (push sym args-processed)
				   (push sym args-lambda)))
			       (push i args-processed))
	 finally (return (values (reverse args-processed) (reverse args-lambda))))))
