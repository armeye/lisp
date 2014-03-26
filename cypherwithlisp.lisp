(defun reverse-cypher (msg)
  (reverse msg))

(reverse-cypher "123")

(defconstant +letters+ "abcdefghijklmnopqrstuvwxyz")

(defun get-char-with-shift (c shift)
  (let ((pos (position c +letters+)))
    (if pos
	(schar +letters+ (mod (+ pos shift) 26))
	c)))

(defun ceasar-encrypt (str shift)
  (map-into (make-string (length str)) (lambda (c) (get-char-with-shift c shift)) str))

(defun ceasar-decrypt (str shift)
  (ceasar-encrypt str (- 0 shift)))

(ceasar-encrypt (ceasar-encrypt "ab c" 28) -28)


(defun transposition-encrypt (msg key)
  "Transpose characters in msg by size of key"
  (declare (string msg)
	   (optimize speed)
	   ((integer 1 *) key))
  (multiple-value-bind (dim rest) (floor (length msg) key)
    (declare (integer dim rest))
    (unless (zerop rest) (incf dim)) ; adjust array dimensions
    (let ((arr (make-array `(,dim ,key) :element-type 'character :displaced-to (if (zerop rest) 
										   msg 
										   (fill-string msg (* dim key))))) 
	  (res (make-array (length msg) :element-type 'character :fill-pointer 0)))
      (dotimes (j key res)
	(dotimes (i dim)
	  (let ((c (aref arr i j)))
	    (unless (char= c #\Nul) (vector-push c res))))))))

(defun fill-string (src new-string-size)
  "Helper function to adjust string to a multiple of key size"
  (let ((array (make-array new-string-size :element-type 'character :fill-pointer 0)))
    (loop for c across src do (vector-push c array)) array))

(transposition-encrypt "Common sense is not so common." 8)

(defun transposition-decrypt (msg key)
  
)

(ceiling 10 8)
