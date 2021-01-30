;;; Extensions to xlisp

;;; Russell G. Almond---almond@stat.washington.edu


;;;  Makes a change to the require/provide behavior to better support
;;;  prototyping code.

;;; Changes in the early part of a system could well make objects
;;; in later files depend directly or indirectly on out of date
;;; prototypes.  To insure changes are probagated properly, we should
;;; re-read and module read after the last one changed.  

;;; The new-provide  changes the behavior of provide to accomplish
;;; this maintenance.  If the tag is in the list of modules, all
;;; modules after the tag are removed.  This encourages the reloading
;;; of affected code.  If the tag is not in the list of modules,
;;; pushed onto the list.

;;; Equality test is #'equal to accomodate both strings Luke's
;;; prefered module style and keywords, my prefered module style.

(load (strcat ElToY-directory "utils" *d-sep*  "require"))

(defun new-provide (tag)
  "Works like provide only strips later read files off of modules so
any dependent file is re-read"
  (let ((tail (member tag *modules* :test #'equal)))
    (if tail (setq *modules* tail)
      (push tag *modules*))))


(provide :new-provide)
