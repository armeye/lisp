(require 'ironclad)
(require 'alexandria)
(require 'asdf)
(require 'split-sequence)

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
		  (filetype :type string :accessor key-filetype :initform "")
		  (path :type string :accessor key-path :initform "")))

(defmethod initialize-instance :after ((k key) &key (file nil) (str nil))
  (assert (not (and file str)))
  (flet ((fill-in-key-path ()
	   (let ((pathhash (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :md5 (key-hash k)))))
	     (setf (key-path k) (format nil "~A/~A" (subseq pathhash 0 3) (subseq pathhash 3 6)))))
	 (fill-in-key-size ()
	   (setf (key-size k) (if (streamp file)
	       (file-length file)
	       (with-open-file (s file)
		 (file-length s))))))
    (when file
      (let ((digest (ironclad:digest-file :tiger file)))
	(setf (key-digest k) :tiger)
	(fill-in-key-size)
	(setf (key-hash k) digest)
	(setf (key-filetype k) (pathname-type file))
	(fill-in-key-path)))
    
    (when str
      (destructuring-bind (d s h) (split-sequence:split-sequence #\- (pathname-name str))
	(declare (ignore d))
	(setf (key-filetype k) (pathname-type str))
	(setf (key-digest k) :tiger)
	(setf (key-hash k) (ironclad:hex-string-to-byte-array h))
	(setf (key-size k) (parse-integer s))
	(fill-in-key-path)))))

(defmethod print-object ((k key) stream)
  (format stream "Hash:~A Filetype:~A Size:~D Path:~A~%" (ironclad:byte-array-to-hex-string (key-hash k))
	  (key-filetype k)
	  (key-size k)
	  (key-path k)))


(defun build-filename-from-key (k)
  (format nil "~A/~A-~D-~A.~A"
	  (key-path k)
	  (symbol-name (key-digest k))
	  (key-size k)
	  (ironclad:byte-array-to-hex-string (key-hash k))
	  (key-filetype k)))

(defun move-file-into-storage (name k &key (keep-original nil))
  (funcall (if keep-original
	       #'alexandria:copy-file
	       #'rename-file)
	   name (ensure-directories-exist (merge-pathnames
					   "~/annex/.git/annex/objects/" (build-filename-from-key k)))))

(merge-pathnames "test/123.txt" "~/.git/annex/objects/")
(move-file-into-storage)
(truename ".")

(print (make-instance 'key :str "SHA256E-1234-e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
(print (make-instance 'key :file "xfile03.mp4"))

(alexandria:copy-file)

(pathname-name "/test/foo.mp3")

(uiop:copy-file)
