;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


(require :el-bootstrap (strcat ElToY-directory "Dist-Lib" *d-sep*  "Bootstrap.lsp"))

(defvar *CLT-Families* *Known-families*)

;;; extra clt demo families

(defun normalize (vals)
  "Normalize list of values by dividing by their sum. *Vectorized*"
  (/ vals (sum vals)))


(def wavy-dist
     (send finite-family :new :name :wavy
	   :atoms (iseq 1 9)
	   :mass (normalize '(2 4 2 1 3 5 6 5 3))))

(push wavy-dist *CLT-Families*)

(def outlier-dist (send finite-family :new :name :outliers
		    :atoms (nconc (iseq 1 7) '(21 22))
		    :mass (normalize '(1 2 3 5 3 2 1 1 1))))

(push outlier-dist *CLT-Families*)

(def holey-dist (send bootstrap-family :new :name :holey
		      :data '( -4 -3 -3 -2 2 3 3 4)))

(push holey-dist *CLT-Families*)

(new-provide :el-clt-families)
