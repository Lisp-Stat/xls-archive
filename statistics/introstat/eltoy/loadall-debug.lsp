
;;; list of toy objects to load


#-WINDOWS (progn (format t "Dist-Toy requires WINDOWS Feature~%")
		 (error "Try again after fixing X window environment~%")
		 )


#+MACINTOSH (progn (format t "Small programming environment~%")
		   (format t "Not loading :documentation ~%")
		   (send *object* :add-method :documentation
			 #'(lambda (&rest args)
			     (format nil "Documentation Disabled~%"))))

(def *d-sep* "/")
(def ElToY-directory "/bass/users/almond/ElToY/")
(debug)
(expand 5)
(load (strcat ElToY-directory "utils" *d-sep*  "new-provide"))
(load (strcat ElToY-directory "toys" *d-sep*  "disttoy"))
(expand 1)
(load (strcat ElToY-directory "clt" *d-sep*  "cltplot"))
(load (strcat ElToY-directory "toys" *d-sep*  "el-menubar"))
(expand 4)
(load (strcat ElToY-directory "toys" *d-sep*  "eltoy"))
(defun reload (&rest args)
  (mapc #'load args)
  (load (strcat ElToY-directory "toys" *d-sep*  "eltoy")))

