;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the global proto-type for an elicitation display
;;; tool. 


(require :el-conj-mixin (strcat ElToY-directory "conj" *d-sep*  "conj-mixin.lsp"))
(require :el-distdisp (strcat ElToY-directory "fam" *d-sep*  "distdisp.lsp"))

;
;;; pri-post-proto --- extended prototype for prior and posterior distributions
(defproto pri-post-disp-proto
  '(marginal-tool			;marginal distribution display
					;tool (if this is a prior
					;distribution)
    parent-signal
    ) '() (list dist-disp-proto inh-conjugate-family-mixin)
"This is a prior/posterior distribuiton display tool. 
	:spawn-prediction -- spawns a new predictive data interval
"
)

(defmeth pri-post-disp-proto :isnew (&rest args
				     &key (parent-signal :hyperparameters)
				     (parent-signal-fun #'pass-as-parameters)
				     &allow-other-keys)
  (apply #'call-next-method args)
  (setf (slot-value 'marginal-tool) nil)
  (send self :parent-signal parent-signal parent-signal-fun)
  self)

(send pri-post-disp-proto :documentation :isnew
      (strcat
"pri-post-disp-proto: :parent-signal  --- signal to parent when
parameters change.
"
      (send dist-disp-proto :documentation :isnew)
      (send inh-conjugate-family-mixin :documentation :isnew)))


(defmeth pri-post-disp-proto :destruct (&rest args)
  (when (slot-value 'marginal-tool)
	(send (slot-value 'marginal-tool) :destruct)
	(setf (slot-value 'marginal-tool) nil))
  (setf (slot-value 'parent-signal) nil)
  (apply #'call-next-method args))


;;; update --- this mostly passes messages back and forth to the
;;; parent tool and the two display tools, extra display tool
(defmeth pri-post-disp-proto :update (&rest args)
  (apply #'call-next-method args)
  (when (slot-value 'marginal-tool)
	(if (find :update (trace))
	    (format t ":update (pri-post-disp-proto) sending to marginal-tool~%")
	(apply #'send (slot-value 'marginal-tool) :update args))))


(defmeth pri-post-disp-proto :spawn-marginal (&rest args)
  (if (slot-value 'marginal-tool)
      (cerror "Ignore message." "Marginal Tool already exists.")
  (format t "Warning: :spawn-marginal unimplimented.")))
(send pri-post-disp-proto :documentation :spawn-marginal
      "Creates a marginal tool.")
  


(defmeth pri-post-disp-proto :parameters
  	(&optional (signal :values) &rest args)
  (when (find :parameters (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (if (find :parameters (trace))
	  (format t ":parameters (pri-post-disp-proto) :local doing nothing~%"))
    ;; not a local call
    (let ((set (if (member signal (send self :parameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (progn
	    	(if (find :parameters (trace))
		    (format t ":parameters (inh-parameter-mixin) <set>=T passing to supertool~%"))
		(apply #'send  self :supertool (slot-value 'parent-signal)
		       signal args))
	(progn
	  (if (find :parameters (trace))
	      (format t ":parameters (inh-parameter-mixin) <set>=nil handling locally~%"))
	  (apply #'send  (slot-value 'parameters) signal args))))))

(send pri-post-disp-proto :documentation :parameters
      "Sends to parent using :parent-signal.  On :local does nothing."
      
)

;; :parent-signal
(defmeth pri-post-disp-proto :parent-signal (&optional (signal nil set)
						   (signal-fun
						    #'pass-as-parameters
						    psf-given))
   (when set
	 (when (and (slot-value 'parent-signal)
		    (send self :has-method (slot-value 'parent-signal)))
	       (unless psf-given
		       (setq signal-fun (send self :get-method
					      (slot-value 'parent-signal))))
	       (send self :remove-method (slot-value 'parent-signal)))
	 (setf (slot-value 'parent-signal) signal)
					; take care of local parameter updating
	 (send self :add-method signal signal-fun))
   (slot-value 'parent-signal))
  
(send pri-post-disp-proto :documentation :parent-signal
      ":parent-signal &optional signal signal-fun

Returns/sets parent-signal.  

If setting, the new <signal> is made a method executing the
<signal-fun>.
")


(defun pass-as-parameters (self &optional (signal :values)
				     &rest args)
  (let ((trace (find (send self :parent-signal) (trace))))
    (when trace (format t "Object ~S" self))
    (if (eq signal :local)		;local set of some nature
	(apply #'send self :update :parameters args)
    ;non-local set, send to parents
      (let ((set (if (member signal (send (slot-value 'paras) :names))
		     (or (> (list-length args) 1) (numberp (car args)))
		   args)))
	(when trace
	      (format t "parent-signal: signal:~S  set:~S args:~S ~%"
		      signal set args ))
	(if set
	    (apply #'send  self :supertool
		   (send self :parent-signal) signal args)
	  (apply #'send  (slot-value 'paras) signal args))))))


;
;;; margin-proto --- prototype for marginal distributions
;(defproto margin-proto
;  '(data-names				;names of data
;    prior-family			;prior family function
					;form
;    ) '() (list dist-disp-proto nui-param-mixin)
;"This is a marginal distribuiton display tool. Some non-obvious
;messages it accepts:
;	:nui-parameter :rescale -- rescales the parameters
;"
;)

;;; nuicance parameters are meant to be things like "N" in the
;;; binomial distribution.



(new-provide :el-ppdistdisp)
