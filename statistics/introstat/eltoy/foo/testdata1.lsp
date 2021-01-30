
;;; Test object for data and nui-parameters

(require :el-paradisp (strcat ElToY-directory "params" *d-sep*  "paradisp.lsp"))
(require :el-nui-parameters (strcat ElToY-directory "params" *d-sep*  "nui-parameters.lsp"))
(require :el-conj-mixin (strcat ElToY-directory "conj" *d-sep*  "conj-mixin.lsp"))
(require :el-Norm-Norm (strcat ElToY-directory "Conj-Lib" *d-sep*  "Norm-Norm.lsp"))
(require :el-Beta-Binomial (strcat ElToY-directory "Conj-Lib" *d-sep*  "Beta-Binomial.lsp"))
(require :el-data-mixin (strcat ElToY-directory "params" *d-sep*  "data-mixin.lsp"))
(require :el-data-disp (strcat ElToY-directory "params" *d-sep*  "data-disp.lsp"))

(defproto 2data-tool-proto '(data-disp1
			      data-disp2) '()
			      (list
			            el-data-mixin
				    nui-parameter-mixin
				    conjugate-family-mixin
				    ))


(defmeth 2data-tool-proto :print (&optional (stream t))
  (format stream "#<2data-TOOL (~A)>"
	  (if (send self :conjugate-family)
	      (send self :conjugate-family :name))))

(defmeth 2data-tool-proto :isnew (&rest args)
  (prog1 
      (apply #'call-next-method args)
    (setf (slot-value 'data-disp1)
	  (send data-nui-para-disp-proto :new '()
		:title "Parmeter/Data Display 1"
		:parameters (send self :nui-parameter-object)
		:parent-signal :nui-parameters
		:data (send self :data-object)
		:supertool self))
    (setf (slot-value 'data-disp2)
	  (send data-nui-para-disp-proto :new '()
		:title "Parmeter/Data Display 2"
		:parameters (send self :nui-parameter-object)
		:parent-signal :nui-parameters
		:data (send self :data-object)
		:supertool self))		
    ))

(defmeth 2data-tool-proto :destruct (&rest args)
  (send (slot-value 'data-disp1) :destruct)
  (setf (slot-value 'data-disp1) nil)
  (send (slot-value 'data-disp2) :destruct)
  (setf (slot-value 'data-disp2) nil)
  (apply #'call-next-method args))

(defmeth 2data-tool-proto :update (signal &rest data)
  (apply #'call-next-method signal data)
  (when (slot-value 'data-disp1)
	(if (find :update (trace))
	    (format t ":update (2data-tool-proto) send to 1st display~%"))
	(apply #'send (slot-value 'data-disp1) :update signal data))
  (when (slot-value 'data-disp2)
	(if (find :update (trace))
	    (format t ":update (2data-tool-proto) send to 2nd display~%"))
	(apply #'send (slot-value 'data-disp2) :update signal data)))


(defun make-2data-tool (family)
  (send 2data-tool-proto :new
	:conjugate-family family 
	:nui-parameter-names (send family :nui-parameter-names)
	:nui-parameter-values (send family :nui-default-parameters)
	:nui-parameter-range (send family :nui-parameter-range-default)
	:nui-parameter-limits (send family :nui-parameter-limits)
	:nui-parameter-integer? (send family :nui-parameter-integer?)
	:nui-parameter-granularity (send family :nui-parameter-granularity)
	:nui-parameter-constraint-fun
	      (send family :nui-parameter-constraint-fun)
	:data-names (send family :data-names)
	:data-limits (send family :data-limits)
	:data-constraint-fun (send family :data-constraint-fun)
	:data-values (send family :default-data)
	:data-integer? (send family :data-integer?)))



(setq *b-fam* (send Beta-Binomial-Family :new))
(setq *n-fam* (send Normal-Normal-Family :new))
(setq *2pt* (make-2data-tool *b-fam*))
(setq *p-obj* (send *2pt* :nui-parameter-object))
(setq *d-obj* (send *2pt* :data-object))
(setq *d1-obj* (send *2pt* :slot-value 'data-disp1))
(setq *d2-obj* (send *2pt* :slot-value 'data-disp2))
(setq *f-obj* (send *2pt* :conjugate-family))

