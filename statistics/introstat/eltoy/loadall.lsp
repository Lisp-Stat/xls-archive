
;;; list of toy objects to load

#-WINDOWS (progn (format t "Dist-Toy requires WINDOWS Feature~%")
		 (format t "Try again after fixing X window environment~%")
		 (exit))

#+MACINTOSH (progn (format t "Small programming environment~%")
		   (format t "Not loading :documentation ~%")
		   (send *object* :add-method :documentation
			 #'(lambda (&rest args)
			     (format nil "Documentation Disabled~%"))))


#-MACINTOSH(def *d-sep* "/")
#+MACINTOSH(def *d-sep* ":")
(def ElToY-directory "/bass/users/almond/ElToY/" )
(expand 5)
(load (strcat ElToY-directory "toys" *d-sep*  "disttoy"))
(expand 1)
(load (strcat ElToY-directory "clt" *d-sep*  "cltplot"))
(load (strcat ElToY-directory "toys" *d-sep*  "el-menubar"))

(defun load-eltoy ()
  (expand 4)
  (load (strcat ElToY-directory "toys" *d-sep*  "eltoy"))
  (load (strcat ElToY-directory "toys" *d-sep*  "el-menubar")))

