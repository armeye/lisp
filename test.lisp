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

(defmacro curry (f val &optional (fnam nil))
  (if fnam
      `(defun ,fnam (&rest x) (apply ,f ,val x))
      `(lambda (&rest x) (apply ,f ,val x))))

(defmacro compose (f1 f2)
  `(lambda (x) (funcall ,f1 (funcall ,f2 x))))

(defun take (list n)
  "Take first n elements form list"
  (subseq list 0 n))

(defun drop (list n)
  "Drop first n elements from list"
  (subseq list n))

(defun take-right (list n)
  "Take last n elements from list"
  (subseq list (- (length list) n)))

(defun drop-right (list n)
  "Drop last n elements from list"
  (subseq list 0 (- (length list) n)))

(defun split-at (list n)
  "Split list in two at n"
  (values (take list n) (drop list n)))

(defmacro with-collectors (collectors &body rest)
  "Provide a set of collectorsto a function body"
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

;(named-let foo ((a 1) (b 0)) 
;  (if (= b 10) 
;      a 
;      (foo (+ a 20) (1+ b))))

(defun split-args-and-init (list)
  (loop for (arg init) in list 
     collect arg into arglist 
     collect init into initlist 
     finally (return (values arglist initlist))))
