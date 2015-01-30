; Copyright (c) 2013-2014 Arne Meyer <meyer.arne83@gmail.com>
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice unmodified, this list of conditions, and the following
;    disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defun reverse-cypher (msg)
  (reverse msg))

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
