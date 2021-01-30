;; 
;; I don't know about Unix, but on the Mac, wildcard arguments to XlispStat's
;; directory function only seem to work for an entire field, that is,
;;  (DIRECTORY "*.dat") works but (DIRECTORY "xyz*.dat") doesn't. 
;; 
;; The following function uses a wildcard function for directory, and 
;; then filters on another match string. 
;; ( Ought to add an option for regexp matching later. ) 
;; 

( defun filematch (dir &optional (wildpat "*") pat)
"Args: directory &optional wildcard-pattern match-pattern
 [directory can be a single directory pathname or a list of pathnames]"
  (when (eq wildpat T) (setf wildpat "*"))
  (if (listp dir)
      (combine 
       (remove nil
               (mapcar #'(lambda (d) (filematch d wildpat pat)) dir)))
  (let* ((wild (make-pathname :directory (truename dir) :name wildpat))
         (files (directory wild)))
    (if (null pat) files 
       (remove pat files :test-not #'string-search)))))


;; Recursive directory listing seems to be slow, at least partially due to
;; no posix isdir function, but my primary need is for building file lists 
;; for batch operations, where speed is not a big concern.  


( defun dirs  ( toplevel &optional recurse )
"Args: toplevel &optional recurse
 RECURSE can be Nil, T or positive-integer number of levels" 
  ( let* (( wild (make-pathname :directory (truename toplevel) :name "*" ))
          ( dirs (set-difference (directory wild :all T )
                                 (directory wild )
                                 :test #'string= )))
;; There's no standard lisp isdir function, so we use the fact that 
;;  #'DIRECTORY returns only regular files unless :ALL T switch 
;; and assume the set difference -- all files that aren't regular files
;; are directories. 
    (cond 
      ((null dirs) dirs)
      ((null recurse)  dirs )
      (( and (numberp recurse) (zerop recurse)) dirs )
      ( T 
         (append dirs 
                  (remove Nil (combine 
                               (mapcar 
                               #'(lambda (x) 
                                   (dirs x 
                                         (if (numberp recurse) (1- recurse) T)))
                                dirs))))))))





