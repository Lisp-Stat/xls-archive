#|

bayeslin.lsp

BAYES-LIN 0.1 alpha 2
(C) 1997 Darren J Wilkinson

This file may be distributed in unmodified form, and may be modified
for personal use. It may not be distributed in modified form.
Suggested modifications should be sent to me for inclusion in the
next release.

d.j.wilkinson@newcastle.ac.uk
http://www.ncl.ac.uk/~ndjw1/


    BAYES-LIN WWW page:        http://www.ncl.ac.uk/~ndjw1/bayeslin/

BAYES-LIN documentation is available from the BAYES-LIN WWW page.

|#





(require "blin-tnp")
(require "blin-mnp")
; (require "blin-tpp") ; re-hash completely!
(require "blin-mpp")

(provide "bayeslin")


;; ***************************************************************

;; declare global variables to contain the node names
(def *tree-nodes* NIL)
(def *moral-nodes* NIL)


;; **************************************************************


;; now define a few global functions

;; tree node initialisation function
(defun create-tree-node (name variables ex var neighbours)
"Args: (name variables ex var neighbours)
Creates a tree-node object."
;; add object to the node list
  (setf *tree-nodes* (cons name *tree-nodes*))
;; create the object
  (setf (symbol-value name) (send tree-node-proto :new))
;; initialise the slots
  (send (symbol-value name) :slot-value 'name name)
  (send (symbol-value name) :slot-value 'variables variables)
  (send (symbol-value name) :slot-value 'expectation ex)
  (send (symbol-value name) :slot-value 'variance var)
  (send (symbol-value name) :slot-value 'neighbours neighbours)
)

;; moral node initialisation function
(defun create-moral-node (name variables tree-node print-name neighbours)
"Args: (name variables tree-node print-name neighbours)
Creates a moral-node-object."
;; add the object to the node list
  (setf *moral-nodes* (cons name *moral-nodes*))
;; create the object
  (setf (symbol-value name) (send moral-node-proto :new))
;; initialise the slots
  (send (symbol-value name) :slot-value 'name name)
  (send (symbol-value name) :slot-value 'variables variables)
  (send (symbol-value name) :slot-value 'tree-node tree-node)
  (send (symbol-value name) :slot-value 'print-name print-name)
  (send (symbol-value name) :slot-value 'neighbours neighbours)
  (send (symbol-value name) :slot-value 'var_b_inv
	(ginv (send (symbol-value name) :var)))
  (send (symbol-value name) :slot-value 'ex_b
	(send (symbol-value name) :ex))
)


;; tree plot initialisation function
;; THIS STUFF IS TO BE MADE OBSOLETE!!!!!!!!!!
(defun create-tree-plot (name)
"Args: (name)
Creates a tree-plot object."
;; create the object
  (setf (symbol-value name) (send tree-plot-proto :new))
;; initialise the slots
  (send (symbol-value name) :slot-value 'nodes
	*tree-nodes*)
  (send (symbol-value name) :slot-value 'resolutions
	(repeat nil (length *tree-nodes*)))
  (send (symbol-value name) :slot-value 'size-ratios
	(repeat nil (length *tree-nodes*)))
;; make some nice colors for shading with
  (make-color 'outline 0 0 0)
  (make-color 'arc 0 0 0)
  (make-color 'node-back 0.97 0.97 0.93)
  (dotimes (i 10)
    (make-color (intern (format nil "~a" i)) (first (uniform-rand 1)) (first (uniform-rand 1)) (first (uniform-rand 1)) )
    )
)

;; moral plot initialisation function
(defun create-moral-plot (name &optional (nodes *moral-nodes*))
"Args: (name &optional (nodes *moral-nodes*))
Creates a moral graph plot object with the given name."
;; create the object
(setf (symbol-value name) (send moral-plot-proto :new))
(send (symbol-value name) :title (format nil "BAYES-LIN Moral plot - ~a" name))
;; initialise the slots
(send (symbol-value name) :slot-value 'nodes nodes)
(send (symbol-value name) :slot-value 'diagnostics t)
(send (symbol-value name) :slot-value 'node-labels t)
(send (symbol-value name) :slot-value 'outlines t)
;; make some nice colours for shading with
  (make-color 'outline 0 0 0)
  (make-color 'arc 0.8 0.8 0.8)
  (make-color 'node-back 0.9 0.9 0.9)
  (make-color 'node 0.1843 0.3098 0.3098) ; DarkSlateGray
  (dotimes (i (length nodes))

    (make-color (intern (format nil "~a" i))
		(+ 0.75 (/ (random 64) 256))
		(+ 0.75 (/ (random 64) 256))
		(+ 0.75 (/ (random 64) 256))
		)
    )
)


;; global moral plot initialisation function
(defun create-global-moral-plot (name &optional (nodes *moral-nodes*))
"Args: (name &optional (nodes *moral-nodes*))
Creates a moral graph plot object with the given name."
;; create the object
(setf (symbol-value name) (send global-moral-plot-proto :new))
(send (symbol-value name) :title (format nil "BAYES-LIN Global moral plot - ~a" name))
;; initialise the slots
(send (symbol-value name) :slot-value 'nodes nodes)
(send (symbol-value name) :slot-value 'diagnostics t)
(send (symbol-value name) :slot-value 'node-labels t)
(send (symbol-value name) :slot-value 'outlines t)
;; make some nice colours for shading with
  (make-color 'outline 0 0 0)
  (make-color 'arc 0.8 0.8 0.8)
  (make-color 'node-back 0.9 0.9 0.9)
  (make-color 'node 0.1843 0.3098 0.3098) ; DarkSlateGray
  (dotimes (i (length nodes))

    (make-color (intern (format nil "~a" i))
		(+ 0.75 (/ (random 64) 256))
		(+ 0.75 (/ (random 64) 256))
		(+ 0.75 (/ (random 64) 256))
		)
    )
)


;; a generalised inverse function is needed
;; botch it for the moment!!!!
;; FIX THIS!!!!
(defun ginv (mat)
"Args: (matrix)
Returns the Moore-penrose generalised inverse of matrix.
ACTUALLY - IT JUST RETURNS AN INVERSE AT THE MOMENT."
(inverse mat)
)


(defun latex-matrix (mat &optional (s t))
"Args: (matrix &optional (stream t))
Prints a LaTeX version of matrix to stream."
(let* ((dim (array-dimensions mat))
       (r (first dim))
       (c (second dim)))
  (format s "~&\\left(\\begin{array}{")
  (dotimes (i c)
    (format s "r")
    )
  (format s "}~%")
  (dotimes (rr r)
    (if (> rr 0)
	(format s "\\\\~%"))
    (dotimes (cc c)
      (if (> cc 0)
	  (format s "\&"))
      (format s "~,2f" (select mat rr cc))
      )
    )
  (format s "~%\\end{array}\\right)~%")
  )
nil
)



;; print a friendly little header....

(format t "
BAYES-LIN 0.1 (Alpha).
Copyright (c) 1997, by Darren Wilkinson.

")


;; end


