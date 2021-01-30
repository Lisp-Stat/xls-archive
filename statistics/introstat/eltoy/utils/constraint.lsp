;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines cont-uniform distribution families.

(require :new-provide (strcat ElToY-directory "utils" *d-sep*  "new-provide.lsp"))
(require :el-superset (strcat ElToY-directory "utils" *d-sep*  "superset.lsp"))


;;; Constraint functions are functions which check constraints on
;;; parameters and data.  There are two types of constraint functions,
;;; parameter constraint functions and data constraint functions.
;;; They return t if their arguments follow the logical constraints
;;; and nil otherwise.  An optional keyword argument :warn (defaults
;;; to t) prints a warning message describing the failure.  These
;;; functions are applied to parameter and data objects to check
;;; validity. 






;;; ------------------- Parameter Constraint Functions  -----------------

;;; Parameter constraint functions take a single argument which should
;;; be a parameter object.  


(defun vacuous-constraint (parameters &key (warn t) &allow-other-keys)
  "This constraint function is always satisfied."
  t)


(defun increasing-constr-fun (parameters &key (warn t)
					     &allow-other-keys)
  (cond ((apply #'> parameters)
	   (if warn (warn "Parameters not in increasing order ~A~%"
			  parameters))
	   nil)
	(t t)))


(defun decreasing-constr-fun (parameters &key (warn t)
					     &allow-other-keys)
  (cond ((apply #'< parameters)
	   (if warn (warn "Parameters not in decreasing order ~A~%"
			  parameters))
	   nil)
	(t t)))



; 
;;; ----------------- Data Constraint Functions ----------------


;;; Data constraint functions take two arguments, the data and the
;;; nuisance parameters.  They should be appropriately vectorized.


(defun vacuous-data-constraint (data nuiparameters
				     &key (warn t) &allow-other-keys)
  "This constraint function is always satisfied."
  t)

(defun max-data-constraint (data nuiparameters
				     &key (warn t) &allow-other-keys)
  "Constrain data values to be less than nuisance parameters. 
Assumes there is one nuisance parameter for each data parameter.
"
  (let ((result (every #'identity (element-seq (<= data nuiparameters)))))
    (if (and warn (not result))
	(warn "Data ~S bigger than upper bound ~S~%" data nuiparameters))
    result))



(new-provide :el-constraint)

