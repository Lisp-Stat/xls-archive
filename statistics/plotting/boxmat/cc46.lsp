(send *data-set-menu* :new-title "cc46-eelworms")
(def block (transpose (matrix '(3 5) (list

'(4 2 4 4 3 2 2 1 3 2 3 1 4 3 1 1)
'( ) '( ) '( ) '( )

'( )
'(4 3 2 1)
'(4 2 1 3)
'(2 3 4 1)
'(2 4 3 1)

'( )
'(4 2 1 3)
'(4 2 3 1)
'(4 2 3 1)
'(2 4 3 1)

))))


(def fct (transpose (matrix '(3 5) (list

'( 19 25 29 44 67 74 88 100 102 134 191 197 209 211 216 269)
'( ) '( ) '( ) '( )

'( )
'( 19 42 89 252)
'( 23 62 138 194)
'( 41 107 153 212)
'(107 109 124 230)

'( )
'( 9 42 145 193)
'( 17 127 128 282)
'( 48 95 162 263)
'( 80 193 222 283)

))))


(def sct (transpose (matrix '(3 5) (list

'(106 212 254 268 338 137 356 219 363 590 563 421 352 505 708
466)
'( ) '( ) '( ) '( )

'( )
'(114 222 332 398)
'( 80 221 194 433)
'(176 415 454 386)
'(236 132 268 256)

'( )
'( 92 308 304 561)
'( 28 166 311 372)
'(298 199 365 379)
'(142 292 408 280)

))))

(def non-nil-sct (remove nil (combine sct)))
(def min (min non-nil-sct))
(def max (max non-nil-sct))
(def mean (mean non-nil-sct))
(def min-mean (- min mean))
(def max-mean (- max mean))

(def dose     (col-indices sct))
(def fumigant (row-indices sct))
(def d*f      (interaction-indices dose fumigant))
(def b*d*f    (interaction-indices block d*f))

(def sct-mean	  (minus sct mean))		  ;df 47
(def block-effect (wilk-sweep sct-mean block))	  ;df  3
(def sct-block	  (minus sct-mean block-effect))  ;df 44

(def b*d*f-effect (wilk-sweep sct-block b*d*f))   ;df 32
(def w-b*d*f	  (minus sct-block b*d*f-effect)) ;df 12

(def d*f-effect  (wilk-sweep b*d*f-effect d*f))   ;df  8
(def b*d*f-resid (minus b*d*f-effect d*f-effect)) ;df 24

(def resid36	 (minus sct-block d*f-effect))	  ;df 36


;covariate analysis
(def non-nil-fct (remove nil (combine fct)))
(def min.f (min non-nil-fct))
(def max.f (max non-nil-fct))
(def mean.f (mean non-nil-fct))
(def min-mean.f (- min mean.f))
(def max-mean.f (- max mean.f))


(defun cc46-reg ()
 (let* ((x-bl  (dummy-vars (non-nil block)))
	(x-fct (non-nil fct))
	(x-d*f (dummy-vars (non-nil d*f  )))
	(y     (non-nil sct))
	(reg-0 (regression-model
		 (bind-columns x-bl x-fct x-d*f)
		 y
		 :print nil
	       )
	)
	(x-matrix (send reg-0 :x-matrix))
	(qr-obj (qr-decomp x-matrix))
	(x-ortho (select qr-obj 0))
       )
       (def cc46-reg-obj (regression-model x-ortho y :intercept nil :print nil))
 )
)

(cc46-reg)
(def bhat (send cc46-reg-obj :coef-estimates))
(def ss-vec (^ bhat 2))
(print (sum (select ss-vec '(1 2 3))))
(print (sum (select ss-vec '(4))))
(print (sum (select ss-vec '(5 6 7 8 9 10 11 12))))
(print (send cc46-reg-obj :residual-sum-of-squares))


(def x-ortho	(send cc46-reg-obj :x-matrix))
(def all-rows	(iseq 0 47))
(def x-d*f	(select x-ortho all-rows '(5 6 7 8 9 10 11 12)))
(def bhat-d*f	(select bhat '(5 6 7 8 9 10 11 12)))
(def d*f.x	(matmult x-d*f bhat-d*f))
(def resid35	(send cc46-reg-obj :residuals))
(def fct-effect (matmult (select x-ortho all-rows 4) (select bhat 4)))
(def sct-block.x (+ d*f.x resid35))


(def fct-mean	      (minus fct mean.f))
(def fct-block-effect (wilk-sweep fct-mean block))
(def fct-block	      (minus fct-mean fct-block-effect))

(def non-nil-sct-b (remove nil (combine sct-block)))
(def non-nil-fct-b (remove nil (combine fct-block)))


(defun box46 (data &key (title "boxmat cc46")
			(variable-labels '( "dose" "fumigant" ))
			(x-axis-label '(0 1 2))
			(row-names '("Cntl" "cn" "cs" "cm" "ck"))
			(min min-mean)
			(max max-mean)
			(df 0)
			(margin '(12 -2 -35 -9))
			(range-detail 100)
	     )
   (boxmat data :title title
		:variable-labels variable-labels
		:x-axis-label x-axis-label
		:row-names row-names
		:min min
		:max max
		:df df
		:margin margin
		:range-detail range-detail
   )
)

(def b.sct	 (box46 sct	     :title "sct"       :df 47
 :min min :max max))

(def b.sct-block (box46 sct-block    :title "sc.adj.block" :df 44))


(def tmp (remove nil
		 (combine w-b*d*f b*d*f-effect d*f-effect b*d*f-resid resid36)))

(def b.sct-block.x
  (box46 (array-list sct sct-block.x) :title "sc.adj.block.x" :df 43
	 :min (min tmp) :max (max tmp)))

(def b.resid35
  (box46 (array-list sct resid35)   :title "resid35"    :df 35
	 :min (min tmp) :max (max tmp)))

(def b.block	 (box46 block-effect :title "block"     :df  3))

(def b.fct-effect
  (box46 (array-list sct fct-effect) :title "fct-effect" :df  1
	 :min (min tmp) :max (max tmp)))

(def b.d*f.x
  (box46 (array-list sct d*f.x)     :title "d*f.x"      :df  8
	 :min (min tmp) :max (max tmp)))
