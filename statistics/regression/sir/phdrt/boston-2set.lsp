
 (load "data-boston")

 (setf case (append (iseq 0 355) (iseq 488 505)))

 (setf lowcrime (subset data-b case))

 (def x(select lowcrime (iseq 1 13)))

 (def y(car lowcrime))


 
 (setf case1 (iseq 356 487))

 (setf highcrime (subset data-b case1))

 (def x1(select highcrime (iseq 1 13)))

 (def y1(car highcrime))


; run the "outlier-sresq" function --
;          (reg    (regression-model (list px (^ px 2)) res :print nil))
;          (sres   (send reg :externally-studentized-residuals))
