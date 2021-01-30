;;; Damn it Luke, if your going to implement common lisp
;;; functionality, do it right!!!!

(defun require (tag &optional (path tag))
"Args: (tag &optional (path tag))
Loads module NAME, unless it has already been loaded. If PATH is supplied it
is used as the file name; otherwise NAME is used. If file NAME is not in the
current directory *default-path* is searched."
  (let ((name (string tag))
        (path (string path)))
    (unless (member tag *modules* :test #'equal)
            (if (load path)
                t
#+macintosh     (let ((vol (set-volume)))
                  (unwind-protect (load path)
                                  (set-volume vol)))
#-macintosh     (load (strcat *default-path* path))))))
