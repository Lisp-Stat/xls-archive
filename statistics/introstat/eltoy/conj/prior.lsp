;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy and use this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the distribution family prototypes for an elicitation tool

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))
;;; prior /posterior families

(defproto prior-family-proto 
  '() '() (list family-proto)
"Differences for prior distribution family"
)


(send prior-family-proto :name :prior)


;;; --- Discrete-family-proto ---

(defproto discrete-prior-proto '() '()
  (list prior-family-proto discrete-family-proto)
  "Discrete probabilty distribution specialization")

(send discrete-prior-proto :name :|Disc-Prior|)


;;; --- continuous-family-proto ---

(defproto continuous-prior-proto '() '()
  (list prior-family-proto continuous-family-proto)
  "Discrete probabilty distribution specialization")

;; to get things to print right.
(send continuous-prior-proto :name :|Cont-Prior|)


;;; --- Undefined-prior ---

(defproto undefined-prior-family '() '()
  (list undefined-family prior-family-proto))


(new-provide :el-prior)
