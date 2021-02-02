(defun printmods (alltrans.out)
"Reformats output from alltrans 

***********************************************************************
Version 1.1: November 15, 1996
 
This software is not formally maintained, but I would be happy to hear
from people who have problems with it.
 
Permission is hereby granted to StatLib to redistribute this software.
The software can be freely used for non-commercial purposes, and can
be freely distributed for non-commercial purposes only.
The copyright is retained by the developer.
Copyright 1996 Jennifer A. Hoeting
***********************************************************************

INPUTS
   alltrans.out = output from alltrans
"

(let* (
       (l.vars (select alltrans.out 0))
       (l.trans (combine (select l.vars 0) (remove 0 (select alltrans.out 1))))
       (trans.list (select alltrans.out 2))
       (n.models (length l.vars))
       (p (round (/ (log n.models) (log 2))))

       ;COMPUTE BINARY FORM OF VARIABLE SELECTION ONLY MODELS
       (a (repeat (order l.vars) (repeat p n.models)))
       (b (rem (floor (/ a (repeat (** 2 (iseq p)) n.models))) 2))

       ;FORMAT OUTPUT FOR VARIABLE SELECTION ONLY MODELS
       (mods.list (repeat (+ (iseq p) 1) n.models))
       (m (- (length b) (sum b)))

       ;COMPUTE BINARY FORM OF TRANSFORMATION MODELS
       (a2 (repeat (order l.trans) (repeat p n.models)))
       (b2 (rem (floor (/ a2 (repeat (** 2 (iseq p)) n.models))) 2))
       (transmods (matrix (list n.models p) b2))

       ;CREATE LIST OF TRANSFORMATIONS 
       (i (combine (select (select alltrans.out 2)
			   (order (remove 0 (select alltrans.out 1))))))
       (trans2 (combine transmods))
       (m.trans (length (which (= trans2 0))))
       (mods.list.trans (repeat (+ (iseq p) 1) n.models))
       )

(setf (select mods.list (which (= b 0))) (repeat " " m))
(setf (select mods.list.trans (which (= trans2 0))) (repeat " " m.trans))
(setf (select trans2 (which (= trans2 1))) i)
(setf (select trans2 (which (= trans2 0))) (repeat " " m.trans))

;FORMAT OUTPUT
(let* ( 
       (v1 (bind-columns (matrix (list n.models p) mods.list)
			    (sort-data l.vars)))
       (v2 (combine (repeat " " p) "     L_m"))
       (v3 (bind-rows v2 v1))

       (t1 (bind-columns (matrix (list n.models p) trans2)
                            (sort-data l.trans)))
       (t2 (combine (repeat " " p) "     L_m"))
       (t3 (bind-rows t2 t1))
       )


(print "VARIABLE SELECTION ONLY MODELS")
(terpri)
(print-matrix v3)

(print "VARIABLE AND TRANSFORMATION SELECTION MODELS")
(terpri)
(print-matrix t3)

)
)
)

