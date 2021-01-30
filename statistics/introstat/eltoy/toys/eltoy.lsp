;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the global prototype for an elicitation tool

(require :el-conj-lib (strcat ElToY-directory "Conj-Lib" *d-sep*  "Conj-Lib.lsp"))
(require :el-tool (strcat ElToY-directory "toys" *d-sep*  "el-tool.lsp"))



(defun make-el-tool (conj &rest args
			  &key (title nil)
			  &allow-other-keys)
  (apply #'send el-tool-proto :new
	 :conjugate-family conj 
	 :title (if title title
		  (format nil "ElToY: ~A" (send conj :name)))
	 args))

(defvar *eltoy* (send menu-proto :new "ElToY")
  "Menu of available conjugate distribution toys.")

(send *eltoy* :append-items
      (send menu-item-proto :new "---Distribution---" 
	    :function #'sysbeep))

(defun toggle-eltoy (family)
; (declare (type Family family))
  "Toggles presence/absense of ElToY for conjugate distribuition <family>"
  (let ((d-toy (get '*eltoy* (send family :name))))
    (if d-toy
	(progn
	  (send d-toy :destruct)
	  (remprop '*eltoy* (send family :name)))
      (progn
	(putprop '*eltoy*
		 (make-el-tool (send family :new))
		 (send family :name))))))


(defun launch-ElToY ()
  (mapcar #'add-el-toy *Known-conjugate-families*)
  (send *ElToY* :install))

(defun add-el-toy (family)
  (send *ElToY* :append-items
	(send menu-item-proto :new (format nil "~A" (send family :name))
	      :action (function (lambda ()
				  (toggle-eltoy family))))))

(launch-ElToY)

(new-provide "ElToY")
