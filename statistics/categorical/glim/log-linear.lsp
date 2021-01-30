;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Loglinear model fitting in the poissonreg-proto
;;
;; Just say 
;; 
;; (log-linear-model ncat counts model vars)
;;
;; where ncat is a list with the number of categories of 
;; the variables, counts is a list of cell counts, model
;; is a list of lists indicating which interactions go
;; in the model, for example '((0 1) (2 3)) or '((0) (0 1)),
;; and the optional argument vars is a list of variable 
;; names, for example '(pre post effect).
;;
;; This file also provides the utility expand-hierarchy,
;; which expands a hierarchical model to a list of all
;; interactions. Thus
;; 
;; (expand-hierarchy '((0 1) (2 3)))
;;
;; returns
;;
;; ((0) (1) (2) (3) (0 1) (2 3))
;;
;; which can then be used as an argument to log-linear-model.
;;
;; In addition we use utilities for decoding and encoding
;; vectors, and for lexicographical ordering of lists of
;; indices. They are in the separate files decode-encode.lsp
;; and lexico-sort.lsp.
;;
;; Version 1.0, Barcelona, Spain, Jan de Leeuw, April 27, 1995
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "glim")
(require "decode-encode")
(require "lexico-sort")

(defproto log-linear-proto 
  '(model ncat) nil poissonreg-proto)

(defmeth log-linear-proto :isnew (the-ncat the-counts the-model the-vars) 
  (send self :set-model the-model)
  (send self :set-ncat the-ncat)
(call-next-method 
 :y the-counts
 :x (send self :make-pred the-model)
 :predictor-names (send self :make-names the-model the-vars))
)

(defmeth log-linear-proto :make-pred (model)
(let* ((nc (send self :set-ncat))
       (uu (all-profile-list nc))
       (xx nil))
(dolist (mm model xx)
(let* ((mn (select nc mm))
       (vv (mapcar #'(lambda (x) (select x mm)) uu))
       (tt (mapcar #'(lambda (x) (decode x mn :zero-special t)) vv)))
  (setf xx (append xx (indicators tt))) ))
))

(defmeth log-linear-proto :make-names (model vars) 
(let* ((nc (send self :set-ncat))
       (uu (all-profile-list nc))
       (xx nil))
(dolist (mm model xx)
(let* ((mn (select nc mm))
       (nn (iseq (length mm)))
       (vv (mapcar #'(lambda (x) (select x mm)) uu)))
(setf xx (append xx (expand-names mm vv vars)))
))))

(defmeth log-linear-proto :set-ncat (&optional (the-ncat nil set))
 (if set (setf (slot-value 'ncat) the-ncat)
     (slot-value 'ncat))
)

(defmeth log-linear-proto :set-model (&optional (the-model nil set))
 (if set (setf (slot-value 'model) the-model)
     (slot-value 'model))
)

(defun log-linear-model 
  (ncat counts model &optional (vars (iseq (length ncat))))
  (send log-linear-proto :new ncat counts model vars))

(defun expand-hierarchy (model)
"Args: model
Takes a hierarchical model MODEL (list of lists) and
expands it to a full model (list of all unique
sublists of MODEL)"
  (let ((ex nil)
        (as (all-subsets (iseq (1+ (max model))))))
(dolist (mm as)
(if (is-subsetp mm model)
    (setf ex (append ex (list mm)))))
(lexico-sort (rest ex))
))

(defun expand-names (comp blow vars)
(let ((n (length comp)))
(if (= n 1)
    (level-names (mapcar #'first blow) 
                 :prefix (elt vars (first comp)))
    (apply #'cross-names
           (mapcar #'(lambda (y) 
                       (level-names 
                        (mapcar #'(lambda (z) (elt z y)) blow)
                        :prefix (elt vars (elt comp y)))) (iseq n))))
))
