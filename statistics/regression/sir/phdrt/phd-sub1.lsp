;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; pHd-sub.lsp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; direction  reg  reg-sh  scal-comp  knot1  knot  lof  set-r2
; 
; term  p-p~m-m  p-plus~p-minus  size-2   size-4  test(split on phd1) 
;
; split c-split (rc-split & rl-split combine-set)  
;
; split1  split2  proj-var  factor  factor1  subset  forward  
;
; sq-sum  lowess  evl-sign  f_test  combine  neighbor  neighbor1  
;
; neighbor2  weight  sub-weight  sub-loc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (defun direction(nphd x res)
        (let*(
                 (dim  (length x))
                 (out  (phd-model x res :print nil))
                 (p1   (nth 0 (send out :eigen-vectors)))
                 (p1x  (send out :phd1))
                 (kt1  (knot1 (mean p1x) p1x res))
                 (nbeta1(cons kt1 p1))
                 (p2   (nth 1 (send out :eigen-vectors)))
                 (p2x  (send out :phd2))
                 (kt2  (knot1 (mean p2x) p2x res))
                 (nbeta2(cons kt2 p2))
                  p3
                  p3x
                 (evl  (send out :eigen-values))
                 (pval (send out :p-values))
             )
     (when (<= dim 2) (setf p3x nil)
                      (setf nbeta3 nil))
     (when (> dim 2) (setf p3  (nth 2 (send out :eigen-vectors)))
                     (setf p3x (send out :phd3))
                     (setf nbeta3 nil)
                   (when (> nphd 2)
                     (setf kt3 (knot1 (mean p3x) p3x res))
                     (setf nbeta3(cons kt3 p3))
                     ))
   (list nbeta1 nbeta2 nbeta3 evl pval p1x p2x p3x)))

   (defun reg(x y)
        (let*(
                (reg1 (reg-model x y :print nil))
                (res  (send reg1 :residuals))
                (r2   (send reg1 :r-squared))
                (sh   (realpart (send reg1 :sigma-hat)))
             )
        (list res r2 sh)))

   (defun reg-sh(x y)
        (let*(
                (reg1 (reg-model x y :print nil))
                (sh   (realpart (send reg1 :sigma-hat)))
                (coef (send reg1 :coef-estimates))
             )   
        (list sh coef)))

    (defun scal-comp(p px res)
        (let*(
                 (reg2  (reg-model (list px (^ px 2)) res :print nil))
                 (beta (send reg2 :coef-estimates))
                 (c1   (nth 1 beta))
                 (c2   (nth 2 beta))
                 (c    (/ c1 (* c2 -2)))
                 (nbeta(cons c p))
             )
         nbeta))

    (defun knot1(k px res)
        (do*(
             (a1 (knot k px res))
             (a2 (list 0 999))
             (a3 (list 0 0))
              c
             )
            ((< (abs (- (nth 1 a1) (nth 1 a2))) .05)
            c)
              (setf a2(knot (car a1) px res))
;              (print (list 'diff-12 (abs (- (nth 1 a1) (nth 1 a2)))))
              (unless (< (abs (- (nth 1 a1) (nth 1 a2))) .05)
                    (setf a3(knot (car a2) px res))
;                    (print (list 'diff-23 (abs (- (nth 1 a2) (nth 1 a3)))))
                    (when (>= (abs (- (nth 1 a2) (nth 1 a3))) .05)
                              (setf a1 a3))
                    (when (<  (abs (- (nth 1 a2) (nth 1 a3))) .05)
                              (setf a1 a2)
                              (setf a2 a3))
              )  
              (if (<= (nth 1 a1) (nth 1 a2))
                  (setf c (car a1))
                  (setf c (car a2)))
           ))

    (defun knot(xbar px res)
    ;xbar mean px
        (let*(
              (case-p (which (>= px xbar)))
              (case-m (which (<  px xbar)))
              (xp (select px case-p))
              (xm (select px case-m))
              (yp (select res case-p))
              (ym (select res case-m))
              outp
              outm
              xbar1
              outpp
              outmm
              cc   
              lofcc
             )
    (when (and (>= (length case-p) 20) (>= (length case-m) 20))
          (setf outp (reg-sh xp yp))
          (setf outm (reg-sh xm ym))
          (when (< (car outp) (car outm))
                (setf xbar1 (mean xm))
                (setf case-mm (which (< xm xbar1)))
            (when (< (length case-mm) 10)
                      (setf cc xbar))
            (when (>= (length case-mm) 10)
                  (setf xmm  (select xm case-mm))
                  (setf ymm  (select ym case-mm))
                  (setf outmm(reg-sh xmm ymm))
                  (setf cc(/ (- (car (nth 1 outmm)) (car (nth 1 outp)))
                          (- (nth 1 (nth 1 outp)) (nth 1 (nth 1 outmm)))))
             ))
 
          (when (>= (car outp) (car outm))
                (setf xbar1 (mean xp))
                (setf case-pp (which (>= xp xbar1)))
            (when (< (length case-pp) 10)
                     (setf cc xbar))
            (when (>= (length case-pp) 10)
                  (setf xpp (select xp case-pp))
                  (setf ypp (select yp case-pp))
                  (setf outpp (reg-sh xpp ypp))
                  (setf cc(/ (- (car (nth 1 outpp)) (car (nth 1 outm)))
                            (- (nth 1 (nth 1 outm)) (nth 1 (nth 1 outpp)))))
            ))
      )
      (when (or (< (length case-p) 20) (< (length case-m) 20))
;            (print 'obs-less-20)
            (setf cc xbar))
 
      (setf lofcc(lof cc px res))
      (when (= lofcc 9999) (setf cc xbar))
        (list cc lofcc)))
 
    (defun lof(k px res)
        (let*(
              (case-p (which (>= px k)))
              (case-m (which (<  px k)))
              (lcase-p (length case-p))
              (lcase-m (length case-m))
               regp
               regm
               s 
              )
;(print (list 'lof-case lcase-m lcase-p))
        (when (or (< lcase-p 20) (< lcase-m 20))
              (setf s 9999))
        (when (and (>= lcase-p 20) (>= lcase-m 20))
              (setf regp (reg-sh (select px case-p) (select res case-p)))
              (setf regm (reg-sh (select px case-m) (select res case-m)))
              (setf s(sq-sum (list (car regp) (car regm)) 
                                   (list lcase-p lcase-m)))
             )
      s))
 
    (defun set-r2(nbeta1 nbeta2 nbeta3 p1x p2x p3x nphd x oldx y)
        (let*(
                out1
                out2
                out3
                out4
               (rs    (reg oldx y))
               (r2-o  (nth 1 rs))
               (sighat-o(nth 2 rs)) 
               (rsq   (repeat 0 (^ 2 nphd)))
               (sighat(repeat 0 (^ 2 nphd)))
               (case  (repeat 0 (^ 2 nphd)))
             )
       (when (= nphd 1)
                       (setf case (size-2  nbeta1 p1x))
                       (setf out1 (p-plus  nbeta1 p1x oldx y))
                       (setf out2 (p-minus nbeta1 p1x oldx y))
                       (setf (nth 0 rsq) (car out2))
                       (setf (nth 1 rsq) (car out1))
                       (setf (nth 0 sighat) (nth 1 out2))
                       (setf (nth 1 sighat) (nth 1 out1)))
       (when (= nphd 2)
                       (setf case (size-4 nbeta1 nbeta2 p1x p2x))
                       (setf out1 (p-p nbeta1 nbeta2 p1x p2x oldx y))
                       (setf out2 (m-p nbeta1 nbeta2 p1x p2x oldx y))
                       (setf out3 (m-m nbeta1 nbeta2 p1x p2x oldx y))
                       (setf out4 (p-m nbeta1 nbeta2 p1x p2x oldx y))
                       (setf (nth 0 rsq) (car out1))
                       (setf (nth 1 rsq) (car out2))
                       (setf (nth 2 rsq) (car out3))
                       (setf (nth 3 rsq) (car out4))
                       (setf (nth 0 sighat) (nth 1 out1))
                       (setf (nth 1 sighat) (nth 1 out2))
                       (setf (nth 2 sighat) (nth 1 out3))
                       (setf (nth 3 sighat) (nth 1 out4)))
       (list rsq case sighat r2-o sighat-o)))

;**********************************
;;; Breiman step estimation function.
    (defun term(ba x y)
        (let*(
                  (dim (length x))
                  (n   (length y))
                  (k   (length ba))
                  (ml  (make-list (* n k) :initial-element 0))
                  (nx  (split-list ml n))
                  set
                  beta
                  reg1
                  coef
              )
        (when (> k 1)
              (dotimes (i k)
                 (setf temp(repeat 0 n))
                 (setf temp(matx-beta (nth i ba) x))
                 (setf set (which (< temp 0)))
                 (setf (select temp set) (repeat 0 (length set)))
                 (setf (nth i nx) temp)
                 )
              (setf beta(append x nx)))
        (when (= k 1) 
                 (setf temp(repeat 0 n))
                 (setf temp(matx-beta ba x))
                 (setf set (which (< temp 0)))
                 (setf (select temp set) (repeat 0 (length set)))
                 (setf beta(cons temp x))) 
        (setf reg1(reg-model beta y :print nil))
        (setf coef(send reg1 :coef-estimates))
        (setf coef(select coef (iseq 0 dim)))
        coef))

;***************
    (defun p-p(nbeta1 nbeta2 p1x p2x x y)
        (let*(    (case1 (intersection (which (>= p1x (car nbeta1)))
                                       (which (>= p2x (car nbeta2)))))
                  (lcase1(length case1))
                  (newx  (transpose (select (transpose x) case1)))
                  (newy  (select y case1))
                  (reg   (reg-model newx newy :print nil))
                  (r2    (send reg :r-squared))
                  (sighat(realpart (send reg :sigma-hat)))
              )
        (list r2 sighat case1)))

    (defun p-m(nbeta1 nbeta2 p1x p2x x y)
        (let*(    (case2 (intersection (which (>= p1x (car nbeta1)))
                                       (which (< p2x  (car nbeta2)))))
                  (lcase2(length case2))
                  (newx  (transpose (select (transpose x) case2)))
                  (newy  (select y case2))
                  (reg   (reg-model newx newy :print nil))
                  (r2    (send reg :r-squared))
                  (sighat(realpart (send reg :sigma-hat)))
              )
        (list r2 sighat case2)))

    (defun m-p(nbeta1 nbeta2 p1x p2x x y)
        (let*(    (case3 (intersection (which (< p1x (car nbeta1)))
                                       (which (>= p2x  (car nbeta2)))))
                  (lcase3(length case3))
                  (newx  (transpose (select (transpose x) case3)))
                  (newy  (select y case3))
                  (reg  (reg-model newx newy :print nil))
                  (r2    (send reg :r-squared))
                  (sighat(realpart (send reg :sigma-hat)))
              )
        (list r2 sighat case3)))

    (defun m-m(nbeta1 nbeta2 p1x p2x x y)
        (let*(    (case4 (intersection (which (< p1x (car nbeta1)))
                                       (which (< p2x  (car nbeta2)))))
                  (lcase4(length case4))
                  (newx  (transpose (select (transpose x) case4)))
                  (newy  (select y case4))
                  (reg   (reg-model newx newy :print nil))
                  (r2    (send reg :r-squared))
                  (sighat(realpart (send reg :sigma-hat)))
              )
        (list r2 sighat case4)))

    (defun p-plus(nbeta1 p1x x y)
        (let*(
                  (case  (which (>= p1x (car nbeta1))))
                  (newx  (transpose (select (transpose x) case)))
                  (newy  (select y case))
                  (reg   (reg-model newx newy :print nil))
                  (r2    (send reg :r-squared))
                  (sighat(realpart (send reg :sigma-hat)))
              )
        (list r2 sighat)))

    (defun p-minus(nbeta1 p1x x y)
        (let*(
                  (case  (which (<  p1x (car nbeta1))))
                  (newx  (transpose (select (transpose x) case)))
                  (newy  (select y case))
                  (reg   (reg-model newx newy :print nil))
                  (r2    (send reg :r-squared))
                  (sighat(realpart (send reg :sigma-hat)))
              )
        (list r2 sighat)))

    (defun size-2(nbeta1 p1x)
        (let*(
                  (l1 (length (which (>= p1x (car nbeta1)))))
                  (l2 (length (which (<  p1x (car nbeta1)))))
             )
        (list l2 l1)))

    (defun size-4(nbeta1 nbeta2 p1x p2x)
        (let*(
                  (l1 (length (intersection
                               (which (>= p1x (car nbeta1))) 
                               (which (>= p2x (car nbeta2))))))
                  (l2 (length (intersection
                               (which (< p1x (car nbeta1)))
                               (which (>= p2x (car nbeta2))))))
                  (l3 (length (intersection
                               (which (< p1x (car nbeta1)))
                               (which (< p2x (car nbeta2))))))
                  (l4 (length (intersection
                               (which (>= p1x (car nbeta1)))
                               (which (< p2x (car nbeta2))))))
              )
        (list l1 l2 l3 l4)))

   (defun split0(x y px1 c1)
    "report Split"
        (let*(
               (case1 (which (>=  px1 c1)))
               (case2 (which (< px1 c1)))
               (x1  (transpose (select (transpose x) case1)))
               (x2  (transpose (select (transpose x) case2)))
               (y1  (select y case1))
               (y2  (select y case2))
               (l1  (length case1))
               (l2  (length case2))
              )
            (print 'case)
            (print (list l2 l1))
       (list x2 y2 x1 y1)))


   (defun split(x y px1 c1)
    "treat px1 as coordinate variable (SUPPORT or CART) but fit it to linear"
    "report Sigma-Hat->linear"
        (let*(
               (case1 (which (>=  px1 c1)))
               (case2 (which (< px1 c1)))
               (x1  (transpose (select (transpose x) case1)))
               (x2  (transpose (select (transpose x) case2)))
               (y1  (select y case1))
               (y2  (select y case2))
               (reg1(reg-sh x1 y1))
               (reg2(reg-sh x2 y2))
               (l1  (length case1))
               (l2  (length case2))
              )
            (print 'case)
            (print (list l2 l1)) 
            (print (list (car reg2) (car reg1)))
            (print (sq-sum (list (car reg2) (car reg1))
                           (list l2 l1)))
       (list x2 y2 x1 y1)))

   (defun c-split(x y px1 c1)
    "treat px1 as coordinate variable (SUPPORT or CART) but fit it to constant"
    "report Sigma-Hat->constant"
        (let*(
               (case1 (which (>=  px1 c1)))
               (case2 (which (< px1 c1)))
               (x1  (transpose (select (transpose x) case1)))
               (x2  (transpose (select (transpose x) case2)))
               (y1  (select y case1))
               (y2  (select y case2))
               (nob1 (length y1))
               (nob2 (length y2))
               (y1_bar (mean y1))
               (y2_bar (mean y2))
               (sgha1(sqrt (/ (%* (- y1 y1_bar)(- y1 y1_bar)) (- nob1 1))))
               (sgha2(sqrt (/ (%* (- y2 y2_bar)(- y2 y2_bar)) (- nob2 1))))
              )
            (print 'case)
            (print (list nob2 nob1))
            (print (list sgha2 sgha1))
            (print (sq-sum (list sgha2 sgha1)
                           (list nob2 nob1)))
       (list x2 y2 x1 y1)))

   (defun rc-split(x y)
   "Testing Robustness by constant fits plus errors for SUPPORT
    CART and pHdrt in Binary Tree-Structured Splitting"
        (let*(
               (nob (length y))
               (y_bar(mean y))
               (sgha (sqrt (/ (%* (- y y_bar)(- y y_bar)) (- nob 1))))
               (noise(* sgha (normal-rand nob)))
               (ny   (+ y_bar noise))
              )
       (print (list nob sgha))
       (list x ny)))

   (defun rl-split(x y)
   "Testing Robustness by linear fits plus errors for SUPPORT
    CART and pHdrt in Binary Tree-Structured Splitting"
        (let*(
               (nob  (length y))
               (out  (reg-model x y :print nil))
               (fit  (send out :fit-values))
               (sgha (realpart (send out :sigma-hat)))
               (noise(* sgha (normal-rand nob)))
               (ny   (+ fit noise))
              )
       (print (list nob sgha))
       (list x ny)))

   (defun err(x y)
   "Squared error loss with linear fit"
        (let*(
               (nob  (length y))
               (y_bar(mean y))
               (out  (reg-model x y :print nil))
               (fit  (send out :fit-values))
               (error (/ (%* (- y_bar fit)(- y_bar fit)) (- nob 1)))
              )
       (print (list nob error))
       ))

   (defun combine-set(t1 t2)
        (let*(
               (nx (transpose (append (transpose (car t1)) 
                                      (transpose (car t2)))))
               (ny (append (nth 1 t1) (nth 1 t2)))
              )
        (list nx ny)))

   (defun split1(x y lst p1 c1)
       "send phd1 as p1 & select x lst (for pHdrt's Binary 
        Tree-Structured Splitting only)"
        (let*(
               (n   (length y))
               (m   (length lst))
               (xx  (transpose (select x lst)))
               (matx   (make-array (list n m) :initial-contents xx))
               (p1x    (%* matx p1))
               (case1  (which (>=  p1x c1)))
               (case2  (which (< p1x c1)))
               (x1  (transpose (select (transpose x) case1)))
               (x2  (transpose (select (transpose x) case2)))
               (y1  (select y case1))
               (y2  (select y case2))
               (reg1(reg-sh x1 y1))
               (reg2(reg-sh x2 y2))
               (l1  (length case1))
               (l2  (length case2))
              )
            (print 'case)
            (print (list l2 l1))
            (print (list (car reg2) (car reg1)))
            (print (sq-sum (list (car reg2) (car reg1))
                           (list l2 l1)))
       (list x2 y2 x1 y1)))

   (defun split2(x y lst p1 p2 beta1 beta2)
       "splitting by using the first two components for pHdrt"
        (let*(
               (n   (length y))
               (m   (length lst))
               (xx  (transpose (select x lst)))
               (matx (make-array (list n m) :initial-contents xx))
               (p1x  (%* matx p1))
               (p2x  (%* matx p2))
               (o1  (p-p beta1 beta2 p1x p2x x y))
               (o2  (m-p beta1 beta2 p1x p2x x y))
               (o3  (p-m beta1 beta2 p1x p2x x y))
               (o4  (m-m beta1 beta2 p1x p2x x y))
              )
            (print 'case)
            (print (list (nth 1 o1) (nth 1 o2) (nth 1 o3) (nth 1 o4)))
            (print (list (nth 2 o1) (nth 2 o2) (nth 2 o3) (nth 2 o4)))
       ))

    (defun proj-var(x y)
        (let*(   
             (out (phdrt-model x x y :print nil))
             (p1x (send out :phd1))
             (fa  (factor x p1x)) 
             (order(car fa)) 
             (rsq  (nth 1 fa)) 
             (case-list(iseq (car (which (>= rsq .95)))))
             case0
             out0
             sighat0
             case1
             out1
             (sighat1 999)
             (case nil)
             (temp-sh nil)
             (case-min nil)
             )
           (if (<= (length case-list) 1)
               (setf case0 (select order '(0 1)))
               (setf case0 (select order case-list))
               ) 
           (setf out0(phdrt-model (select x case0) x y :print nil)) 
           (setf sighat0(send out0 :t-sighat2)) 
           (setf plot (spin-plot (list (send out0 :phd1)
                                       (send out0 :resid)
                                       (send out0 :phd2))
                           :variable-labels (list "phd1" "residuals" "phd2")
                           :title "phd view on first run"
                           :location '(600 600) :size '(280 220)))
                      (send plot :linked t)
                      (send plot :use-color t)
                      (send plot :mouse-mode 'hand-rotate)
                      (send plot :axis-rotate)

       (format t "~%====================================================~%")
       (format t "Information & plot on first running: ~2%")
       (format t "Important variables for pHd: ~s~%" case0)
       (format t "~%====================================================~%")

           (when (> (length case0) 5)
                 (setf p1x (send out0 :phd1))
                 (setf fa  (factor x p1x))
                 (setf order(car fa))
                 (setf rsq  (nth 1 fa))
                 (setf case-list(iseq (car (which (>= rsq .95)))))
                 (if (<= (length case-list) 1) 
                         (setf case1 (select order '(0 1))) 
                         (setf case1 (select order case-list))
                      ) 
                 (setf out1(phdrt-model (select x case1) x y :print nil))
                 (when (eql (send out1 :t-sighat2) nil)
                            (setf sighat1 999))
                 (unless (eql (send out1 :t-sighat2) nil) 
                              (setf sighat1 (send out1 :t-sighat2))) 
           (setf plot1 (spin-plot (list (send out1 :phd1)
                                        (send out1 :resid)
                                        (send out1 :phd2))
                           :variable-labels (list "phd1" "residuals" "phd2")
                           :title "phd view on second run"
                           :location '(600 600) :size '(280 220)))
                       (send plot1 :linked t)
                       (send plot1 :use-color t)
                       (send plot1 :mouse-mode 'hand-rotate)
                       (send plot1 :axis-rotate)

       (format t "~%====================================================~%")
       (format t "Information & plot on second running: ~2%")
       (format t "Important variables for pHd: ~s~%" case1)
       (format t "~%====================================================~%")
       )

      (if (>= sighat0 sighat1)
                (setf case case1)
                (setf case case0))

      (when (<= (length case) 2)
                (setf case-min case)
                (setf case-list (list case))
                (setf temp-sh (list (min sighat0 sighat1)))
                )

      (unless (<= (length case) 2)
                  (setf l-case-1 (- (length case) 1))
                  (setf sh-list  (repeat 0 l-case-1))
                  (setf case-list(repeat 0 l-case-1))
                  (setf (car sh-list) (min sighat0 sighat1))
                  (setf (car case-list) case)
                  (setf case-1 nil)
                  (dotimes (i (- l-case-1 1))
                       (setf case(remove (car (last case)) case))
                       (setf out2(phdrt-model (select x case) x y :print nil))
                       (setf k (+ i 1))
                       (setf (nth k sh-list) (send out2 :t-sighat2)) 
                       (setf (nth k case-list) case)
                       )
                 (dotimes (i (length sh-list))
                          (when (eql (nth i sh-list) nil)
                                (setf case-1(append case-1 (list i)))
                          ))

          (setf temp-sh(remove nil sh-list))
          (setf min-sh(min temp-sh))
          (setf case-list(reverse (set-difference case-list (select 
                                                  case-list case-1))))
          (setf case-min(select case-list (car (which (= min-sh temp-sh)))))
             )
;(print (list 'phd-var case-min))
    (list case-min case-list temp-sh)))
 

   (defun factor(x phd)
        (let*(
              (dim (length x))
              reg1
              r2
              temp
              (set (iseq 0 (- dim 1)))
              case
              (r2-ls(repeat 0 dim))
              (ls   (iseq 1 (- dim 1)))
              case1
              )
        (setf temp (repeat 0 dim))
        (dotimes (i dim)(setf reg1(reg-model (nth i x) phd :print nil))
                        (setf r2 (send reg1 :r-squared))
                        (setf (nth i temp) r2)
                        )
        (setf case(position (max temp) temp))
        (setf (car r2-ls) (max temp))
        (setf set(remove case set))
        (dolist (k ls)
           (setf temp (repeat 0 dim))
           (dolist (j set)
                      (setf reg1(reg-model (select x
                                (combine case j)) phd :print nil))
                      (setf r2 (send reg1 :r-squared))
                      (setf (nth j temp) r2)
                      )
        (setf case1(position (max temp) temp))
        (setf set  (remove case1 set))
        (setf (nth k r2-ls) (max temp))
        (setf case (combine case case1))
        )
        (list case r2-ls)))

   (defun factor1(x phd case)
        (let*(
              (dim (length x))
              reg1
              r2
              temp
              (set  (iseq 0 (- dim 1)))
              case1
              (dc   (- dim (length case)))
              (r2-ls(repeat 0 dc))
              (ls   (iseq 0 (- dc 1)))
              )
        (setf set(remove case set))
        (dolist (k ls)
         (setf temp (repeat 0 dim))
         (dolist (j set)
                         (setf reg1(reg-model (select x
                                  (combine case j)) phd :print nil))
                         (setf r2 (send reg1 :r-squared))
                         (setf (nth j temp) r2)
                         )
         (setf case1(position (max temp) temp))
         (setf set  (remove case1 set))
         (setf (nth k r2-ls) (max temp))
         (setf case (combine case case1))
        )
        (list case r2-ls)))

   (defun subset(x case)
        (setf nx(transpose (select (transpose x) case)))
     nx) 

   (defun forward(x y)
       (let*(
              (dim (length x))
              reg1
              ratio
              case
              case1
              temp
              (set (iseq 0 (- dim 1)))
              (ratio-ls(repeat 0 dim))
              (ls  (iseq 2 dim))
              )
        (setf temp (repeat 0 dim))
        (dotimes (i dim)(setf reg1(regression-model (nth i x) y :print nil))
                        (setf ratio(/ (send reg1 :coef-estimates)
                                      (send reg1 :coef-standard-errors)))
                        (setf (nth i temp) (abs (nth 1 ratio)))
                        )
        (setf case(position (max temp) temp))
        (setf (car ratio-ls) (max temp))
        (setf set(remove case set))
        (dolist (k ls)
         (setf temp (repeat 0 dim))
         (dolist (i set)(setf reg1(regression-model
                                  (select x (combine case i)) y :print nil))
                         (setf ratio(/ (send reg1 :coef-estimates)
                                       (send reg1 :coef-standard-errors)))
                         (setf (nth i temp) (abs (nth k ratio)))
                         )
         (setf case1(position (max temp) temp))
         (setf set  (remove case1 set))
         (setf (nth (- k 1) ratio-ls) (max temp))
         (setf case (combine case case1))
         )
(print (list case ratio-ls))
        (list case ratio-ls)))

   (defun sq-sum0(lst)
      (let*( (out(%* lst lst))
             )
       out))

   (defun sq-sum(sighats lst)
;lst means a list of # of cases, sighats means a list of sigma-hat.
        (let*(
              (obs  (length lst))
              (nsig (repeat 0 obs))
              (w 0)
              out
             )
           (dotimes (i obs) (if (equal (nth i sighats) nil)
                                (setf w 99999)
                                (setf (nth i nsig)(** (nth i sighats) 2))))
           (if (/= w 99999) 
               (setf out(sqrt (/ (%* nsig lst) (sum lst))))
               (setf out nil))
      out))

;************
   (defun evl-sign(x y)
        (let*(
               (res (car (reg x y)))
               (srxx (covw x (- res (mean res))))
               (sxx  (covw x (repeat 1 (length res))))
               (s-m2 (car (chol-decomp sxx)))
               (inv-s-m2 (inversep2 s-m2))
               (m3   (matmult (matmult inv-s-m2 srxx) (transpose inv-s-m2)))
               (z    (car (eigen m3)))
               (order-z   (reverse (order (abs z))))
               (evl  (select z order-z))
             )
;        (print evl)
        evl))

  (defun f_test(x1 y1 x2 y2)
     (let*(
       (reg0  (reg-model (transpose (append (transpose x1)
                   (transpose x2))) (append y1 y2) :print nil))
       (rss0  (send reg0 :sum-of-squares))
       (df0   (send reg0 :df))
       (reg1  (reg-model x1 y1 :print nil))
       (rss1  (send reg1 :sum-of-squares))
       (df1   (send reg1 :df))
       (reg2  (reg-model x2 y2 :print nil))
       (rss2  (send reg2 :sum-of-squares))
       (df2   (send reg2 :df))
       (dfup  (+ df1 df2))
       (dfdown (- df0 dfup))
       (test  (/ (* (- rss0 (+ rss1 rss2)) dfup) (* (+ rss1 rss2) dfdown)))
       )
      (list test dfdown dfup)))

  (defun combine (&rest args)
     "Args (&rest args)
      Returns sequence of elements of all arguments."
      (copy-seq (element-seq args)))
