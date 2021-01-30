#|

blin-tnp.lsp

(C) Darren J Wilkinson

This file may be distributed in ummodified form, and may be modified
for personal use. It may not be distributed in modified form.
Suggested modifications should be sent to me for inclusion in the
next release.

d.j.wilkinson@newcastle.ac.uk
http://www.ncl.ac.uk/~ndjw1/

|#

(provide "blin-tnp")

;; prototype and methods for junction tree nodes

;; set up a prototype for junction tree nodes
(defproto tree-node-proto
  '(name variables neighbours variance expectation 
	 var-d-inv cov-d-self obs-vars obs-d-ed
	 location)) 

;; now define some methods for the prototype

;; info method
(defmeth tree-node-proto :info (&key full (stream t))
"Method args: (&key full (stream t))
Prints (to :stream) some basic information about the node.
If :full is t, gives full info. Returns nil."
;; basic info
(format stream "~&Junction Tree Node: ~a" (slot-value 'name))
(format stream "~&Variables: ~a" (slot-value 'variables))
(format stream "~&Expectation: ~a" (slot-value 'expectation))
(format stream "~&Variance: ")
(print-matrix (slot-value 'variance) stream :float-digits 3)
(format stream "~&Neighbours: ~a" (slot-value 'neighbours))
;; extra info
(if (and full (slot-value 'obs-vars))
    (list (format stream "~&Observed variables: ~a" (slot-value 'obs-vars))
     (format stream "~&d-E(D): ~a" (slot-value 'obs-d-ed))
     (format stream "~&Var(D)^(-1): ")
     (print-matrix (slot-value 'var-d-inv) stream :float-digits 3)
     (format stream "~&Cov(D,~a): " (slot-value 'name))
     (print-matrix (slot-value 'cov-d-self) stream :float-digits 3)
     )
  )
;; return nil
nil
)

;; positions method
(defmeth tree-node-proto :positions (vars)
"Method args: (vars)
Returns a list of positions of the argument list, as a list."
(let ((allvars (slot-value 'variables)))
(mapcar #'(lambda (x) (position x allvars)) vars)
))

;; expectation method
(defmeth tree-node-proto :ex (&optional (vars (slot-value 'variables)))
"Method args: (&optional (vars (slot-value 'variables)))
Returns an expectation vector for the list of variables supplied
as an argument. Defaults to the full expectation vector for the node."
;; FIX THIS!!
(if (subsetp vars (slot-value 'variables))
    (select (slot-value 'expectation) (send self :positions vars) )
  (make-array (list (length vars)) :initial-element 0)
  )
)

;; covariance method
(defmeth tree-node-proto :cov (v1 v2)
"Method args: (v1 v2)
Returns a covariance matrix for the two variable lists supplied."
(select (slot-value 'variance) (send self :positions v1) (send self :positions v2))
)

;; variance method
(defmeth tree-node-proto :var (&optional (vlist (slot-value 'variables)))
"Method args: (&optional (vlist (slot-value 'variables)))
Returns a variance matrix for the list of variables supplied.
Defaults to the full variance matrix."
;; FIX THIS!
(if (subsetp vlist (slot-value 'variables))
    (send self :cov vlist vlist)
  (identity-matrix (length vlist))
  )
)

;; adjusted expectation method
(defmeth tree-node-proto :aex (&optional (vars (slot-value 'variables)))
"Method args: (&optional (vars (slot-value 'variables)))
Returns an adjusted expectation vector for the list of variables
suppied as an argument, based on unabsorbed data. Defaults to the
full adjusted expectation vector for the node."
;; FIX THIS!
(if (subsetp vars (slot-value 'variables))
    (let ((aexvec (+ (slot-value 'expectation)
		     (%* (transpose (slot-value 'cov-d-self))
			 (slot-value 'var-d-inv)
			 (slot-value 'obs-d-ed))
		     )))
      (select aexvec (send self :positions vars))
      )
  (send self :ex vars)
  )
)

;; adjusted variance method
(defmeth tree-node-proto :avar (&optional (vars (slot-value 'variables)))
"Method args: (&optional (vars (slot-value 'variables)))
Returns the adjusted variance matrix for the list of variables supplied.
Defaults to the full adjusted variance matrix for the node."
(- (send self :var vars)
   (send self :rvar vars)
   )
)

;; resolved variance method
(defmeth tree-node-proto :rvar (&optional (vars (slot-value 'variables)))
"Method args: (&optional (vars (slot-value 'variables)))
Returns the resolved variance matrix for the list of variables supplied.
Defaults to the full adjusted variance matrix for the node."
;; CHECK IT IS OK TO SUBSET RVAR MATRIX, RATHER THAN CONSTRUCT IT FROM SUBSETS
;; SHOULD DO SOMETHING BETTER (CORRECT!) WHEN VARIABLE LISTS OVERLAP
(if (subsetp vars (slot-value 'variables))
    (let ((avarmat (%* (transpose (slot-value 'cov-d-self))
		       (slot-value 'var-d-inv)
		       (slot-value 'cov-d-self))
		   ))
      (select avarmat (send self :positions vars) (send self :positions vars))
      )
  (make-array (list (length vars) (length vars)) :initial-element 0)
  )
)


;; transform method
(defmeth tree-node-proto :transform (&optional (vars (slot-value 'variables)))
"Method args: (&optional (vars (slot-value 'variables)))
Returns the resolution transform matrix for the list of variables. Defaults to
the full transform for the node."
  (let* ((cds (slot-value 'cov-d-self))
	 (cdv (apply #'bind-columns (select (column-list cds) (send self :positions vars))))
	 (var (send self :var vars)))
    (transpose (%* (transpose cdv)
		   (slot-value 'var-d-inv)
		   cdv
		   (ginv var)))
    )
)


;; resolution method
(defmeth tree-node-proto :resolution (&optional (vars (slot-value 'variables)))
(/ (sum (diagonal (send self :transform vars))) (length vars))
)


;; bearing method  ************ DO THIS !!!!! **************

;; size-ratio method
(defmeth tree-node-proto :size-ratio (&optional (vars (slot-value 'variables)))
"Method args: (&optional (vars (slot-value 'variables)))
Returns the size-ratio for the adjustment of the given variables."
(/
(%*
 (- (send self :aex vars) (send self :ex vars))
 (ginv (send self :var vars))
 (- (send self :aex vars) (send self :ex vars))
 )
(length vars))
)

;; observe method
(defmeth tree-node-proto :observe (vars data)
"Method args: (vars data)
Expects a list of variables to be observed, and a vector of
observations. Data slots are updated appropriately. Effect is
propagated around the join tree. Returns nil."
(slot-value 'obs-vars vars)
(slot-value 'obs-d-ed (- data (send self :ex vars)))
(slot-value 'var-d-inv (ginv (send self :var vars)))
(slot-value 'cov-d-self (send self :cov vars (slot-value 'variables)) )
;; now propagate message to neighbours
(dolist (node (slot-value 'neighbours))
(send (symbol-value node) :propagate
      (slot-value 'name)
      (slot-value 'variables)
      (slot-value 'obs-vars)
      (slot-value 'obs-d-ed)
      (slot-value 'var-d-inv)
      (slot-value 'cov-d-self)
      )
)
nil
)

;; remove neighbour method
(defmeth tree-node-proto :remove-neighbour (dead-node)
"Method args: (dead-node)
Removes a dead node from the neighbour list of the
recieving object. Returns nil."
(slot-value 'neighbours (set-difference (slot-value 'neighbours) (list dead-node)))
nil
)

;; absorb method
(defmeth tree-node-proto :absorb (&optional (call-node nil))
"Method args: (&optional (call-node nil))
Send this message to any node on the join tree, and evidence
propagated around the graph by the last :OBSERVE will be 
incorporated into the tree. Observed variables will be removed.
Optional argument for internal use only. Returns nil."
;(format t "~&Absorb occuring at ~a" (slot-value 'name))
;; update expectation and variance matrices
(slot-value 'expectation (send self :aex))
(slot-value 'variance (send self :avar))
;; remove observed variables
(let ((remainder (set-difference (slot-value 'variables) (slot-value 'obs-vars))))
(slot-value 'expectation (send self :ex remainder))
(slot-value 'variance (send self :var remainder))
(slot-value 'variables remainder)
)
;; clear data slots
(slot-value 'obs-vars nil)
(slot-value 'obs-d-ed nil)
(slot-value 'var-d-inv nil)
(slot-value 'cov-d-self nil)
;; if dead, remove this node from neighbours neighbour-lists
(if (eq (slot-value 'variables) nil)
    (dolist (node (slot-value 'neighbours))
      (send (symbol-value node) :remove-neighbour (slot-value 'name))
    )
  )
;; tell neighbours to absorb too
(dolist (node (set-difference (slot-value 'neighbours) (list call-node)))
  (send (symbol-value node) :absorb (slot-value 'name))
  )
nil
)

;; propagation method
(defmeth tree-node-proto :propagate 
  (call-name call-vars obs-vars obs-d-ed var-d-inv cov-d-call)
"Method args: (call-name call-vars obs-vars obs-d-ed var-d-inv cov-d-call)
Used internally to propagate information introduced by :OBSERVE." 
;(format t "~&Node ~a has recieved a message from ~a" (slot-value 'name) call-name)
;; store basic info
(slot-value 'obs-vars obs-vars)
(slot-value 'obs-d-ed obs-d-ed)
(slot-value 'var-d-inv var-d-inv)
;; figure out variables in common, etc.
(let* ((common (intersection call-vars (slot-value 'variables)))
       (distinct (set-difference (slot-value 'variables) common))
       (new (append common distinct))
       )
;; re-order local basis
  (slot-value 'expectation (send self :ex new))
  (slot-value 'variance (send self :var new))
  (slot-value 'variables new)
;; figure out cov-d-self
  (let* ((cov-d-common (select cov-d-call
			       (mapcar #'(lambda (x) (position x obs-vars)) obs-vars)
			       (mapcar #'(lambda (x) (position x call-vars)) common)
			       ))
	 (cov-d-distinct (%* cov-d-common
			     (ginv (send self :var common))
			     (send self :cov common distinct)
			     ))
	 )
    (slot-value 'cov-d-self (bind-columns cov-d-common cov-d-distinct))
    )
  )
;; continue to propagate
(dolist (node (set-difference (slot-value 'neighbours) (list call-name)))
(send (symbol-value node) :propagate
      (slot-value 'name)
      (slot-value 'variables)
      (slot-value 'obs-vars)
      (slot-value 'obs-d-ed)
      (slot-value 'var-d-inv)
      (slot-value 'cov-d-self)
      )
  )
)

;; location accessor method
(defmeth tree-node-proto :location (&optional (loc nil))
"Method args: (&optional (loc nil))
Sets or returns node location (for plotting purposes). Location
is a list of 2 floats on a (0,1) scale."
  (if loc
      (slot-value 'location loc)
    (slot-value 'location)
    )
)

;; ********************************************************************
