;;; normal toy for trying out display features.

(require :el-distdisp (strcat ElToY-directory "fam" *d-sep*  "distdisp.lsp"))
(require :el-normal (strcat ElToY-directory "Dist-Lib" *d-sep*  "Normal.lsp"))

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
		      :family (send supertool :family)
		      :parameters (send supertool :parameter-object)
		      :supertool supertool
		      :title "Normal Distribution"
		      :parent-signal :parameters))
	    :parameter-names (send family :parameter-names)
	    :parameter-values (send family :default-parameters)
	    :parameter-range (send family :parameter-range-default)
	    :parameter-limits (send family :parameter-limits)
	    :family family ))

; (setq norm-tool (make-dist-tool Normal-family))






