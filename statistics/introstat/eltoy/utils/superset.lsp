;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; Xlisp to Common Lisp conversion.  
;;; Damn Xlisp anyway!
;;; Damn Xlisp-stat anyway!

;;; Xlisp is a common lisp subset, the only problem is, of course, how
;;; large a subset.  The purpose of the programs in this file is to
;;; duplicate common lisp functionality not found in xlisp.  So there.

(require :new-provide (strcat ElToY-directory "utils" *d-sep*  "new-provide.lsp"))

;;; list-length
(defun list-length (list)
  (unless (listp list)
	  (error "error:list-length ~S is not a list." list))
  (length list))

;;; list*  --- like list except last element is attached
(defun list* (arg1 &rest args)
  (if (endp args) arg1
    (cons arg1 (apply #'list* args))))


;;; pop and push  
;(defmacro push (item place)
;  `(setf ,place (cons ,item ,place)))

(defmacro pop (place)
  `(prog1 (car ,place) (setf ,place (cdr ,place))))



;;; read-string  --- reads one S-expression from a string
(defmacro read-string (string)
  `(read (make-string-input-stream ,string)))


;;; mapcan --- common list shorthand for the old apply #'append trick. 
(defun mapcan (func list &rest more-lists)
  (apply #'nconc (apply #'mapcar func list more-lists)))



;;; warn --- warning of error
(defun warn (format-string &rest args)
  (apply #'format t format-string args))



;;; second, third, etc

(defun second (lst) (nth 1 lst))
(defun third (lst) (nth 2 lst))
(defun fourth (lst) (nth 3 lst))
(defun fifth (lst) (nth 4 lst))
(defun sixth (lst) (nth 5 lst))
(defun seventh (lst) (nth 6 lst))
(defun eighth(lst) (nth 7 lst))
(defun ninth  (lst) (nth 8 lst))
(defun tenth (lst) (nth 9 lst))


(new-provide :el-superset)

