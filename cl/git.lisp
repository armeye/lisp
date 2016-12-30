;;;(in-package :clannex.git)
(require 'split-sequence)

(defun find-git-executable ()
  (dolist (i (split-sequence:split-sequence #\: (sb-ext:posix-getenv "PATH")))
    (let ((path (format nil "~A/git" i)))
      (when (probe-file path)
	(return path)))))

b(defparameter *git-executable* (find-git-executable))

(defun git (command &rest args)
  (if (= (sb-ext:process-exit-code (sb-ext:run-program *git-executable* (cons command args) :output t))
	 0)
      t
      nil))

(defun symlinkp (pathname)
  (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat pathname))))

(defun init-annex ()
  (unless (and
	   (if (probe-file #p".git")
	       t
	       (and
		(git "init")
		(git "commit" "--allow-empty" "-m" "initial commit")))
	   (ensure-directories-exist ".git/clannex/")
	   (git "checkout" "-b" "clannex")
	   (with-open-file (s ".gitattributes" :direction :output :if-exists :supersede)
	     (print "* merge=union" s))
	   (git "add" ".gitattributes")
	   (git "commit" "-m" "init the annex")
	   (git "checkout" "master"))
    (error "Could not init the annex!")))

(init-annex)
(exit)
