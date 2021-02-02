#|

blin-mnp.lsp

(C) Darren J Wilkinson

This file may be distributed in unmodified form, and may be modified
for personal use. It may not be distributed in modified form.
Suggested modifications should be sent to me for inclusion in the
next release.

d.j.wilkinson@newcastle.ac.uk
http://www.ncl.ac.uk/~ndjw1/

|#

(provide "blin-mnp")

;; prototype and methods for moral graph nodes

;; set up a prototype for moral graph nodes
(defproto moral-node-proto
  '(name variables tree-node neighbours print-name location
	 var_b_inv ex_b resolutions size-ratios global-size-ratios)
  )

;; now define some methods

;; a method to return a little information about the node
(defmeth moral-node-proto :info (&key (stream t))
"Method args: (&key (stream t))
Method to print (to stream) a little information about a node.
Returns nil."
(format stream "~&Moral Graph Node: ~a" (slot-value 'name))
(format stream "~&Print name: ~a" (slot-value 'print-name))
(format stream "~&Variables: ~a" (slot-value 'variables))
(format stream "~&Contained in tree-node: ~a" (send self :tree-node))
(format stream "~&Neighbours: ~a" (slot-value 'neighbours))
(format stream "~&Plot location: ~a" (send self :location))
nil
)

;; a location accessor method
(defmeth moral-node-proto :location (&optional (loc nil))
"Method args: (&optional (loc nil))
Sets or returns node location (for plotting purposes). Location
is a list of 2 floats on a (0,1) scale."
  (if loc
      (slot-value 'location loc)
    (slot-value 'location)
    )
)

;; tree-node accessor method
(defmeth moral-node-proto :tree-node (&optional (node nil))
"Method args: (&optional (node nil))
Sets or returns the encompassing tree-node."
(if node
    (slot-value 'tree-node node)
  (slot-value 'tree-node)
  )
)

;; print-name accessor method
(defmeth moral-node-proto :print-name (&optional (name nil))
"Method args: (&optional (name nil))
Sets or returns the print name."
(if name
    (slot-value 'print-name name)
  (slot-value 'print-name)
  )
)

;; expectation method
(defmeth moral-node-proto :ex (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns an expectation vector for the given variable list.
Defaults to the full expectation vector for the node."
(send (symbol-value (slot-value 'tree-node)) :ex var-list)
)

;; variance method
(defmeth moral-node-proto :var (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns a variance matrix for the given variable list.
Defaults to the full variance matrix for the node."
(send (symbol-value (slot-value 'tree-node)) :var var-list)
)

;; adjusted expectation method
(defmeth moral-node-proto :aex (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns an adjusted expectation vector for the given
variable list. Defaults to the full vector for the node."
(send (symbol-value (slot-value 'tree-node)) :aex var-list)
)

;; adjusted variance method
(defmeth moral-node-proto :avar (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns an adjusted variance matrix for the given
variable list. Defaults to the full matrix for the node."
(send (symbol-value (slot-value 'tree-node)) :avar var-list)
)

;; resolved variance method
(defmeth moral-node-proto :rvar (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns a resolved variance matrix for the given
variable list. Defaults to the full matrix for the node."
(send (symbol-value (slot-value 'tree-node)) :rvar var-list)
)

;; belief transform method
(defmeth moral-node-proto :transform (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns a resolution transform matrix for the given
variable list. Defaults to the full matrix for the node."
(%* (slot-value 'var_b_inv)
    (send self :rvar))
)

;; resolution method
(defmeth moral-node-proto :resolution (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns the resolution for the given variable list for the current
adjustment. Defaults to the overall resolution for the node."
(/
 (sum (diagonal (send self :transform)))
 (length var-list)
 )
)

;; bearing method
(defmeth moral-node-proto :bearing (&optional (var-list 
					       (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns the bearing vector for the given variable list for the current
adjustment. Defaults to the overall bearing for the node."
(send (symbol-value (slot-value 'tree-node)) :bearing var-list)
)

;; size ratio method
(defmeth moral-node-proto :size-ratio (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns the size ratio for the given variable list for the current
adjustment. Defaults to the overall size-ratio for the node."
(/
(%*
 (- (send self :aex var-list) (send self :ex var-list))
; (ginv (send self :var var-list))
 (send self :slot-value 'var_b_inv) ; scale wrt the a priori structure
 (- (send self :aex var-list) (send self :ex var-list))
 )
(length var-list))
)

;; global size ratio method
(defmeth moral-node-proto :global-size-ratio (&optional (var-list (slot-value 'variables)))
"Method args: (&optional (var-list (slot-value 'variables)))
Returns the global size ratio for the given variable list for the
adjustment. Defaults to the overall size-ratio for the node."
(/
(%*
 (- (send self :aex var-list) (slot-value 'ex_b))
 (send self :slot-value 'var_b_inv)
 (- (send self :aex var-list) (slot-value 'ex_b))
 )
(length var-list))
)

;; observe method
(defmeth moral-node-proto :observe (vars data)
"Method args: (vars data)
Expects a list of variables to be observed, and a vector of
observations. Returns nil."
(send (symbol-value (slot-value 'tree-node)) :observe vars data)
)

;; absorb method
(defmeth moral-node-proto :absorb ()
"Method args: ()
Send this to the observed node on the moral graph for current evidence 
to be absorbed into the graph."
;; send the absorb message around the junction tree
(send (symbol-value (slot-value 'tree-node)) :absorb)
)

;; remove neighbour method
(defmeth moral-node-proto :remove-neighbour (node)
"Method args: (node)
Removes the given node from the neighbour list of the recieving node.
Returns nil."
(slot-value 'neighbours
	    (set-difference (slot-value 'neighbours) (list node)))
)



;; ********************************************************************

