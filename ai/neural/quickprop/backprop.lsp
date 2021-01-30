;;; -*- Mode:Lisp -*-

;;; ***************************************************************************
;;; Code to modify Quickprop1 simulator to run standard backprop.

;;; This is loaded on top of Quickprop1 simulator.  It uses standard
;;; backprop for weight updates.  The *MU* parameter is used as momentum
;;; (alpha in normal backprop) and the *EPSILON* parameter is used for
;;; learning rate (conventionally designated epsilon or eta).  The
;;; *PREV-SLOPES* array used by quickprop is ignored here.

;;; Written by Scott E. Fahlman.
;;; ***************************************************************************

;;; This declaration buys a certain amount of overall speed at the expense
;;; of runtime checking.  Comment it out when debugging new, bug-infested code.

(proclaim '(optimize (speed 3) (space 0) (safety 0)))

(defun update-weights ()
  "Update all the weights in the network as a function of each weight's delta
  and previous delta."
  (do ((j *first-hidden* (1+ j)))
      ((= j *nunits*))
    (declare (fixnum j))
    (let ((w (svref *weights* j))
	  (nc (svref *nconnections* j))
          (d (svref *delta-weights* j))
	  (cs (svref *slopes* j)))
      (declare (fixnum nc))
      (dotimes (i nc)
	(declare (fixnum i))
	(incf-sf (svref w i)
		 (setf (svref d i)
		       (+sf (*sf (svref cs i) *epsilon*)
			    (*sf (svref d i) *mu*))))))))



