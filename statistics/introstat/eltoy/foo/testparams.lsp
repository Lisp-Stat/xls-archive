;(load "mylisp")
;(load "normal.lsp")
;(defvar nn (send normal-normal-family :new))


(require :el-paradisp (strcat ElToY-directory "params" *d-sep*  "paradisp.lsp"))
(require :el-para-mixin (strcat ElToY-directory "params" *d-sep*  "para-mixin.lsp"))
(require :el-family-mixin (strcat ElToY-directory "fam" *d-sep*  "family-mixin.lsp"))
(require :el-normal (strcat ElToY-directory "Dist-Lib" *d-sep*  "Normal.lsp"))

(defproto 2param-tool-proto '(param-disp1
			      param-disp2) '()
			      (list 
				    parameter-mixin
				    family-mixin))


(defmeth 2param-tool-proto :print (&optional (stream t))
  (format stream "#<2param-TOOL (~A)>"
	  (if (send self :family)
	      (send self :family :name))))

(defmeth 2param-tool-proto :isnew (&rest args)
  (prog1 
      (apply #'call-next-method args)
    (setf (slot-value 'param-disp1)
	  (send para-disp-proto :new '()
		:title "Parmeter Display 1"
		:parameters (send self :parameter-object)
		:supertool self))
    (setf (slot-value 'param-disp2)
	  (send para-disp-proto :new '()
		:title "Parmeter Display 2"
		:parameters (send self :parameter-object)
		:supertool self))		
    ))

(defmeth 2param-tool-proto :destruct (&rest args)
  (send (slot-value 'param-disp1) :destruct)
  (setf (slot-value 'param-disp1) nil)
  (send (slot-value 'param-disp2) :destruct)
  (setf (slot-value 'param-disp2) nil)
  (apply #'call-next-method args))

(defmeth 2param-tool-proto :update (signal &rest data)
  (apply #'call-next-method signal data)
  (when (slot-value 'param-disp1)
	(if (find :update (trace))
	    (format t ":update (2param-tool-proto) send to 1st display~%"))
	(apply #'send (slot-value 'param-disp1) :update signal data))
  (when (slot-value 'param-disp2)
	(if (find :update (trace))
	    (format t ":update (2param-tool-proto) send to 1st display~%"))
	(apply #'send (slot-value 'param-disp2) :update signal data)))


(defun make-2param-tool (family)
  (send 2param-tool-proto :new
	:family (send family :new)
	:parameter-names (send family :parameter-names)
	:parameter-values (send family :default-parameters)
	:parameter-range (send family :parameter-range-default)
	:parameter-limits (send family :parameter-limits)
	:parameter-integer? (send family :parameter-integer?)
	:parameter-granularity (send family :parameter-granularity)))


(setq *2pt* (make-2param-tool Normal-Family))
(setq *p-obj* (send *2pt* :parameter-object))
(setq *d-obj1* (send *2pt* :slot-value 'param-disp1))
(setq *d-obj2* (send *2pt* :slot-value 'param-disp2))
(setq *f-obj* (send *2pt* :family))
