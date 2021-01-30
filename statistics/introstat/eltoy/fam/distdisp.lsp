;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the global proto-type for an elicitation display
;;; tool. 

(require :el-paramixin (strcat ElToY-directory "params" *d-sep*  "paramixin.lsp"))
(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family-mixin.lsp"))
(require :el-paradisp (strcat ElToY-directory "params" *d-sep*  "paradisp.lsp"))
(require :el-graph (strcat ElToY-directory "fam" *d-sep*  "graph.lsp"))

;;; dist-disp-proto --- primary elicitation tool
(defproto dist-disp-proto 
  '(para-disp				;parameter-display
    dist-graph				;graph
    ) '() (list inh-family-mixin inh-parameter-mixin subtool-mixin
		titled-object) 
"General tool for displaying a distribution attached to a set of
parameters.
"
)


;; generic methods: :describe :print 
(defmeth dist-disp-proto :print (&optional (stream t))
  (format stream "#<Dist-disp: ~S (~S)>"
	  (send self :title)
	  (if (send self :family)
	      (send self :family :name))))

; :describe method
(defmeth dist-disp-proto :describe (&optional (stream t))
   (format stream "A distribution display for the ~S family~%"
	   (send self :family :name))
   (format stream "Current parameter estimates are:")
   (send self :parameters :describe stream nil)
   (format stream "Graph window: ~S~%"
	   (slot-value 'dist-graph)))



;;; :isnew spawns graph and display subwindows

(defmeth dist-disp-proto :isnew
         (&rest args
	  &key (family nil)		;prior distribution family
	       (title nil)
	  &allow-other-keys)
	  

  ; start by initializing simple things
  (apply #'call-next-method :title (if title title
				     (format nil "~S Distrubtion"
					     (send family :name)))
	 args)

;  (setf (slot-value 'family) family) ; inhereted

  ; spawn parameter display
  (setf (slot-value 'para-disp)
	(apply #'send para-disp-proto :new '()
	       :supertool self
	       :parameters (slot-value 'parameters)
	       :title (format nil "~A Parameters" (send self :title))
	       :parent-signal :parameters
	       args))
  
  ; spawn distribution graph
  (setf (slot-value 'dist-graph)
	(apply #'send
	       (send self :family :dist-graph-proto)
	       :new
	       :family (send self :family)
	       :supertool self
	       :parameters (send self :parameter-object)
	       :title (send self :title)
	       args))
  ; return self
  self)



;; destruct -- destroys sub-objects then object itself
(defmeth dist-disp-proto :destruct (&rest args)
;  (setf (slot-value 'family) nil)
  (send (slot-value 'dist-graph) :destruct)
  (setf (slot-value 'dist-graph) nil)
  (send (slot-value 'para-disp) :destruct)
  (setf (slot-value 'para-disp) nil)
  (apply #'call-next-method args))


;;; update --- this mostly passes messages back and forth to the
;;; parent tool and the two display tools.
(defmeth dist-disp-proto :update (&rest args)
  (apply #'call-next-method args)
  (if (find :update (trace))
      (format t ":update (dist-disp-proto) sending to para-disp and dist-graph~%"))
  (apply #'send (slot-value 'para-disp) :update args)
  (apply #'send (slot-value 'dist-graph) :update args))

(defmeth dist-disp-proto :parameter-object (&rest args)
  (prog1 
      (apply #'call-next-method args)
    (when (eql :local (car args))
	  (send (slot-value 'para-disp) :destruct)
	  (setf (slot-value 'para-disp)
		(apply #'send para-disp-proto :new '()
		       self (slot-value 'parameters)
		       :title (format nil "~A Parameters" (send self :title))
		       :parent-signal :parameters
		       args)))))





(new-provide :el-distdisp)

