(require 'ironclad)
(require 'babel)
(require 'split-sequence)
(require :net.didierverna.clon)

(defun file-hash-as-string (name &key (digest :tiger))
  (let 
  ((checksum (ironclad:byte-array-to-hex-string (ironclad:digest-file
				      (ironclad:make-digest digest)
				      name)))
   (filetype (pathname-type name))
   (filelength (with-open-file (s name)
		 (file-length s)))
   (hashname (symbol-name digest)))
    (format nil "~A-s~D-~A.~A" hashname filelength checksum filetype)))

(defclass key () ((digest :type symbol :accessor key-digest :initform nil)
		  (size :type integer :accessor key-size :initform 0)
		  (hash :type (simple-array (unsigned-byte 8) (*)) :accessor key-hash :initform (vector))
		  (filetype :type string :accessor key-filetype :initform "")))

(defmethod initialize-instance :after ((k key) &key (stringkey nil))
  (when stringkey
      (destructuring-bind (d s h) (split-sequence:split-sequence #\- stringkey)
	(setf (key-digest k) (make-symbol d))
	(setf (key-size k) (parse-integer s))
	(setf (key-hash k) (ironclad:hex-string-to-byte-array h)))))

(key-size (make-instance 'key :stringkey "SHA256E-1234-e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))


(defun get-annex-path-from-key (key)
  (let ((hash (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :md5
									   (babel:string-to-octets key)))))
    (format nil "~A/~A" (subseq hash 0 3) (subseq hash 3 6))))

(get-annex-path-from-key "SHA256E-s0--e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

