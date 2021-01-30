;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This creates a menu of distribution display tools for a variety of
;;; distributions.  It is started by the function (launch-dist-toy).
;;;

(require :el-dist-lib (strcat ElToY-directory "Dist-Lib" *d-sep*  "Dist-lib.lsp"))
(require :el-distdisp (strcat ElToY-directory "fam" *d-sep*  "distdisp.lsp"))



(defproto dist-tool-proto '() '() (list mirror-parent-proto
					parameter-mixin
					family-mixin))

(defmeth dist-tool-proto :print (&optional (stream t))
  (format stream "#<DIST-TOOL (~A)>"
	  (if (send self :family)
	      (send self :family :name))))

(defun make-dist-tool (family)
;  (declare (type Family family))
  "Creates a display tool for the given family."
  (send dist-tool-proto :new
	    #'(lambda (supertool)
		(send dist-disp-proto :new
		      :family family
		      :parameters (send supertool :parameter-object)
		      :supertool supertool
		      :title (format nil "~A Distribution" (send family :name))
		      :parent-signal :parameters))
	    :parameter-names (send family :parameter-names)
	    :parameter-values (send family :default-parameters)
	    :parameter-range (send family :parameter-range-default)
	    :parameter-limits (send family :parameter-limits)
	    :parameter-granularity (send family :parameter-granularity)
	    :parameter-integer? (send family :parameter-integer?)
	    :parameter-constraint-fun (send family :parameter-constraint-fun)
	    :family family ))

; (setq norm-tool (make-dist-tool Normal-family))

(defvar *dist-toy* (send menu-proto :new "Dist toy")
  "Menu of available distribution toys.")

(send *dist-toy* :append-items
      (send menu-item-proto :new "---Distribution---" 
	    :function #'sysbeep))


(defun toggle-dist-toy (family)
; (declare (type Family family))
  "Toggles presence/absense of dist-toy for distribuition <family>"
  (let ((d-toy (get '*dist-toy* (send family :name))))
    (if d-toy
	(progn
	  (send d-toy :destruct)
	  (remprop '*dist-toy* (send family :name)))
      (progn
	(putprop '*dist-toy*
		 (make-dist-tool (send family :new))
		 (send family :name))))))


(defun launch-dist-toy ()
  (mapcar #'add-dist-toy *Known-families*)
  (send *dist-toy* :install))


(defun add-dist-toy (family)
  (send *dist-toy* :append-items
	(send menu-item-proto :new (symbol-name (send family :name))
	      :action (function (lambda ()
				  (toggle-dist-toy family))))))

(launch-dist-toy)

