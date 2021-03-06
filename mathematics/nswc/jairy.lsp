(defun jairy (x rx c ai dai) (declare (type double-float x))
 (declare (type double-float rx)) (declare (type double-float c))
 (declare (type double-float ai)) (declare (type double-float dai))
 (prog
  ((dajp (make-array '(19) :element-type 'double-float))
   (dajn (make-array '(19) :element-type 'double-float))
   (da (make-array '(15) :element-type 'double-float))
   (db (make-array '(15) :element-type 'double-float))
   (dak1 (make-array '(14) :element-type 'double-float))
   (dak2 (make-array '(24) :element-type 'double-float))
   (dak3 (make-array '(14) :element-type 'double-float))
   (ajp (make-array '(19) :element-type 'double-float))
   (ajn (make-array '(19) :element-type 'double-float))
   (a (make-array '(15) :element-type 'double-float))
   (b (make-array '(15) :element-type 'double-float))
   (ak1 (make-array '(14) :element-type 'double-float))
   (ak2 (make-array '(23) :element-type 'double-float))
   (ak3 (make-array '(14) :element-type 'double-float)) (con5 0.0d0)
   (scv 0.0d0) (ccv 0.0d0) (cv 0.0d0) (temp2 0.0d0) (e2 0.0d0) (e1 0.0d0)
   (ec 0.0d0) (rtrx 0.0d0) (temp1 0.0d0) (i 0) (f2 0.0d0) (f1 0.0d0) (j 0)
   (tt 0.0d0) (t_ 0.0d0)
  )
  (declare (type (simple-array double-float (*)) dajp))
  (declare (type (simple-array double-float (*)) dajn))
  (declare (type (simple-array double-float (*)) da))
  (declare (type (simple-array double-float (*)) db))
  (declare (type (simple-array double-float (*)) dak1))
  (declare (type (simple-array double-float (*)) dak2))
  (declare (type (simple-array double-float (*)) dak3))
  (declare (type (simple-array double-float (*)) ajp))
  (declare (type (simple-array double-float (*)) ajn))
  (declare (type (simple-array double-float (*)) a))
  (declare (type (simple-array double-float (*)) b))
  (declare (type (simple-array double-float (*)) ak1))
  (declare (type (simple-array double-float (*)) ak2))
  (declare (type (simple-array double-float (*)) ak3))
  (declare (type double-float con5)) (declare (type double-float scv))
  (declare (type double-float ccv)) (declare (type double-float cv))
  (declare (type double-float temp2)) (declare (type double-float e2))
  (declare (type double-float e1)) (declare (type double-float ec))
  (declare (type double-float rtrx)) (declare (type double-float temp1))
  (declare (type fixnum i)) (declare (type double-float f2))
  (declare (type double-float f1)) (declare (type fixnum j))
  (declare (type double-float tt)) (declare (type double-float t_))
  (setq n1 14) (setq m1 12) (setq fpi12 1.3089969)
  (replace ak1 '((- 5.161695E-15)) :end 0)
  (replace ak1 '((- 1.1067954E-14)) :end 0)
  (replace ak1 '(2.0325326E-12) :end 0)
  (replace ak1 '((- 3.3916547E-11)) :end 0)
  (replace ak1 '(1.5396398E-11) :end 0) (replace ak1 '(8.229081E-9) :end 0)
  (replace ak1 '((- 1.29622E-7)) :end 0) (replace ak1 '(3.069026E-7) :end 0)
  (replace ak1 '(1.6382428E-5) :end 0) (replace ak1 '((- 2.3461434E-4)) :end 0)
  (replace ak1 '(8.2284416E-4) :end 0) (replace ak1 '(0.010388116) :end 0)
  (replace ak1 '((- 0.12529024)) :end 0) (replace ak1 '(0.22042309) :end 0)
  (replace ak2 '((- 3.1907704E-15)) :end 0)
  (replace ak2 '(1.19774796E-14) :end 0)
  (replace ak2 '((- 4.507601E-14)) :end 0)
  (replace ak2 '(1.7010604E-13) :end 0)
  (replace ak2 '((- 6.438403E-13)) :end 0)
  (replace ak2 '(2.4446445E-12) :end 0)
  (replace ak2 '((- 9.313883E-12)) :end 0)
  (replace ak2 '(3.5614105E-11) :end 0)
  (replace ak2 '((- 1.3670477E-10)) :end 0)
  (replace ak2 '(5.2686305E-10) :end 0)
  (replace ak2 '((- 2.0390245E-9)) :end 0) (replace ak2 '(7.924547E-9) :end 0)
  (replace ak2 '((- 3.0924536E-8)) :end 0) (replace ak2 '(1.2113041E-7) :end 0)
  (replace ak2 '((- 4.758928E-7)) :end 0) (replace ak2 '(1.8729021E-6) :end 0)
  (replace ak2 '((- 7.368042E-6)) :end 0) (replace ak2 '(2.8876317E-5) :end 0)
  (replace ak2 '((- 1.12124915E-4)) :end 0)
  (replace ak2 '(4.2742753E-4) :end 0) (replace ak2 '((- 0.0015733922)) :end 0)
  (replace ak2 '(0.00539791) :end 0) (replace ak2 '(0.27436614) :end 0)
  (replace ak3 '((- 2.4648031E-16)) :end 0)
  (replace ak3 '(1.6226798E-15) :end 0)
  (replace ak3 '((- 1.1270514E-14)) :end 0)
  (replace ak3 '(8.312858E-14) :end 0)
  (replace ak3 '((- 6.5637524E-13)) :end 0)
  (replace ak3 '(5.6047792E-12) :end 0)
  (replace ak3 '((- 5.2440826E-11)) :end 0)
  (replace ak3 '(5.471384E-10) :end 0) (replace ak3 '((- 6.5229435E-9)) :end 0)
  (replace ak3 '(9.211815E-8) :end 0) (replace ak3 '((- 1.6324997E-6)) :end 0)
  (replace ak3 '(4.0342256E-5) :end 0) (replace ak3 '((- 0.0017812704)) :end 0)
  (replace ak3 '(0.28027144) :end 0) (replace ajp '(2.3579825E-16) :end 0)
  (replace ajp '((- 3.298103E-15)) :end 0)
  (replace ajp '((- 4.966296E-14)) :end 0) (replace ajp '(6.361234E-13) :end 0)
  (replace ajp '(8.253588E-12) :end 0) (replace ajp '((- 9.603594E-11)) :end 0)
  (replace ajp '((- 1.0471634E-9)) :end 0) (replace ajp '(1.0963945E-8) :end 0)
  (replace ajp '(9.7024376E-8) :end 0) (replace ajp '((- 9.0398675E-7)) :end 0)
  (replace ajp '((- 6.163166E-6)) :end 0) (replace ajp '(5.0456478E-5) :end 0)
  (replace ajp '(2.4313763E-4) :end 0) (replace ajp '((- 0.0017274956)) :end 0)
  (replace ajp '((- 0.004954247)) :end 0) (replace ajp '(0.030534273) :end 0)
  (replace ajp '(0.03014126) :end 0) (replace ajp '((- 0.18435636)) :end 0)
  (replace ajp '(0.0778953) :end 0) (replace ajn '(2.3616132E-15) :end 0)
  (replace ajn '((- 2.6776272E-14)) :end 0)
  (replace ajn '((- 4.660226E-13)) :end 0)
  (replace ajn '(4.7246543E-12) :end 0) (replace ajn '(7.208023E-11) :end 0)
  (replace ajn '((- 6.4418615E-10)) :end 0)
  (replace ajn '((- 8.44284E-9)) :end 0) (replace ajn '(6.5296476E-8) :end 0)
  (replace ajn '(7.152188E-7) :end 0) (replace ajn '((- 4.66875E-6)) :end 0)
  (replace ajn '((- 4.1066207E-5)) :end 0) (replace ajn '(2.1831183E-4) :end 0)
  (replace ajn '(0.0014474441) :end 0) (replace ajn '((- 0.005925356)) :end 0)
  (replace ajn '((- 0.02634763)) :end 0) (replace ajn '(0.07493301) :end 0)
  (replace ajn '(0.16582063) :end 0) (replace ajn '((- 0.24531955)) :end 0)
  (replace ajn '(0.038049787) :end 0) (replace a '((- 9.990039E-16)) :end 0)
  (replace a '(4.5565646E-15) :end 0) (replace a '((- 2.9090773E-15)) :end 0)
  (replace a '((- 1.2034083E-13)) :end 0) (replace a '(9.575232E-13) :end 0)
  (replace a '((- 2.3235767E-12)) :end 0)
  (replace a '((- 2.5709141E-11)) :end 0) (replace a '(3.2866082E-10) :end 0)
  (replace a '((- 1.0368574E-9)) :end 0) (replace a '((- 1.8682476E-8)) :end 0)
  (replace a '(2.9815735E-7) :end 0) (replace a '(1.3591608E-7) :end 0)
  (replace a '((- 9.6619595E-5)) :end 0) (replace a '(0.0015764728) :end 0)
  (replace a '(0.4902754) :end 0) (replace b '((- 3.326563E-16)) :end 0)
  (replace b '((- 2.8820512E-15)) :end 0) (replace b '(2.877248E-14) :end 0)
  (replace b '((- 1.1052918E-13)) :end 0)
  (replace b '((- 2.0464483E-13)) :end 0) (replace b '(5.8300717E-12) :end 0)
  (replace b '((- 3.5084997E-11)) :end 0)
  (replace b '((- 3.5509814E-11)) :end 0) (replace b '(2.6094809E-9) :end 0)
  (replace b '((- 2.0010031E-8)) :end 0) (replace b '((- 1.1241591E-7)) :end 0)
  (replace b '(4.7131784E-6) :end 0) (replace b '((- 2.3114968E-5)) :end 0)
  (replace b '((- 0.003529157)) :end 0) (replace b '(0.27859354) :end 0)
  (setq n1d 14) (setq m1d 12) (replace dak1 '((- 1.3914014E-14)) :end 0)
  (replace dak1 '(2.2381092E-13) :end 0) (replace dak1 '(4.2880418E-13) :end 0)
  (replace dak1 '((- 7.4302384E-11)) :end 0)
  (replace dak1 '(1.1309803E-9) :end 0)
  (replace dak1 '((- 5.361943E-10)) :end 0)
  (replace dak1 '((- 2.183112E-7)) :end 0)
  (replace dak1 '(3.0239771E-6) :end 0)
  (replace dak1 '((- 6.356363E-6)) :end 0)
  (replace dak1 '((- 2.700165E-4)) :end 0) (replace dak1 '(0.003121835) :end 0)
  (replace dak1 '((- 0.008498458)) :end 0)
  (replace dak1 '((- 0.06613228)) :end 0) (replace dak1 '(0.20456785) :end 0)
  (replace dak2 '((- 2.6278691E-15)) :end 0)
  (replace dak2 '(9.620051E-15) :end 0)
  (replace dak2 '((- 3.526796E-14)) :end 0)
  (replace dak2 '(1.2949825E-13) :end 0)
  (replace dak2 '((- 4.7630384E-13)) :end 0)
  (replace dak2 '(1.7551013E-12) :end 0)
  (replace dak2 '((- 6.4801393E-12)) :end 0)
  (replace dak2 '(2.3977332E-11) :end 0)
  (replace dak2 '((- 8.892521E-11)) :end 0)
  (replace dak2 '(3.3062042E-10) :end 0)
  (replace dak2 '((- 1.2324906E-9)) :end 0)
  (replace dak2 '(4.6072577E-9) :end 0)
  (replace dak2 '((- 1.7271763E-8)) :end 0)
  (replace dak2 '(6.493078E-8) :end 0)
  (replace dak2 '((- 2.4473266E-7)) :end 0)
  (replace dak2 '(9.2436244E-7) :end 0)
  (replace dak2 '((- 3.4952825E-6)) :end 0)
  (replace dak2 '(1.3209068E-5) :end 0)
  (replace dak2 '((- 4.9745744E-5)) :end 0)
  (replace dak2 '(1.8578643E-4) :end 0)
  (replace dak2 '((- 6.8229757E-4)) :end 0)
  (replace dak2 '(0.0024254017) :end 0)
  (replace dak2 '((- 0.008061968)) :end 0) (replace dak2 '(0.29333234) :end 0)
  (replace dak3 '(2.5537477E-16) :end 0)
  (replace dak3 '((- 1.6852314E-15)) :end 0)
  (replace dak3 '(1.1737476E-14) :end 0)
  (replace dak3 '((- 8.6858824E-14)) :end 0)
  (replace dak3 '(6.8857436E-13) :end 0)
  (replace dak3 '((- 5.908891E-12)) :end 0)
  (replace dak3 '(5.563577E-11) :end 0)
  (replace dak3 '((- 5.853253E-10)) :end 0)
  (replace dak3 '(7.0592563E-9) :end 0)
  (replace dak3 '((- 1.0141849E-7)) :end 0)
  (replace dak3 '(1.8490729E-6) :end 0)
  (replace dak3 '((- 4.8348113E-5)) :end 0)
  (replace dak3 '(0.0025307308) :end 0) (replace dak3 '(0.28467584) :end 0)
  (replace dajp '(7.313275E-17) :end 0)
  (replace dajp '((- 1.1260437E-15)) :end 0)
  (replace dajp '((- 1.587632E-14)) :end 0)
  (replace dajp '(2.2612065E-13) :end 0) (replace dajp '(2.726961E-12) :end 0)
  (replace dajp '((- 3.5724586E-11)) :end 0)
  (replace dajp '((- 3.586737E-10)) :end 0)
  (replace dajp '(4.2961132E-9) :end 0) (replace dajp '(3.4566394E-8) :end 0)
  (replace dajp '((- 3.76344E-7)) :end 0)
  (replace dajp '((- 2.2906788E-6)) :end 0)
  (replace dajp '(2.2582786E-5) :end 0) (replace dajp '(9.428896E-5) :end 0)
  (replace dajp '((- 8.455603E-4)) :end 0)
  (replace dajp '((- 0.0019714613)) :end 0)
  (replace dajp '(0.016794844) :end 0) (replace dajp '(0.009780102) :end 0)
  (replace dajp '((- 0.120262936)) :end 0) (replace dajp '(0.065321915) :end 0)
  (replace dajn '((- 7.328875E-15)) :end 0)
  (replace dajn '(7.3940094E-14) :end 0) (replace dajn '(1.3963176E-12) :end 0)
  (replace dajn '((- 1.2410335E-11)) :end 0)
  (replace dajn '((- 2.0766593E-10)) :end 0)
  (replace dajn '(1.5959992E-9) :end 0) (replace dajn '(2.3268766E-8) :end 0)
  (replace dajn '((- 1.507985E-7)) :end 0)
  (replace dajn '((- 1.8731297E-6)) :end 0)
  (replace dajn '(9.876337E-6) :end 0) (replace dajn '(1.0129733E-4) :end 0)
  (replace dajn '((- 4.1115735E-4)) :end 0)
  (replace dajn '((- 0.0033218702)) :end 0) (replace dajn '(0.00941674) :end 0)
  (replace dajn '(0.05532519) :end 0) (replace dajn '((- 0.08784207)) :end 0)
  (replace dajn '((- 0.31527707)) :end 0) (replace dajn '(0.08533132) :end 0)
  (replace dajn '(0.010859454) :end 0) (replace da '(8.179008E-16) :end 0)
  (replace da '(1.693848E-16) :end 0) (replace da '((- 2.4076525E-14)) :end 0)
  (replace da '(1.6209896E-13) :end 0) (replace da '((- 3.2408912E-13)) :end 0)
  (replace da '((- 3.9965505E-12)) :end 0) (replace da '(4.5372404E-11) :end 0)
  (replace da '((- 1.4475282E-10)) :end 0)
  (replace da '((- 1.8195971E-9)) :end 0) (replace da '(2.872958E-8) :end 0)
  (replace da '((- 6.131589E-8)) :end 0) (replace da '((- 4.617698E-6)) :end 0)
  (replace da '(8.231408E-5) :end 0) (replace da '(0.0031116493) :end 0)
  (replace da '(0.49162734) :end 0) (replace db '((- 7.1179334E-16)) :end 0)
  (replace db '(5.5595063E-15) :end 0) (replace db '((- 1.7725343E-14)) :end 0)
  (replace db '((- 4.9019057E-14)) :end 0) (replace db '(9.64874E-13) :end 0)
  (replace db '((- 5.186742E-12)) :end 0)
  (replace db '((- 4.4099568E-12)) :end 0) (replace db '(3.1699104E-10) :end 0)
  (replace db '((- 2.3637783E-9)) :end 0)
  (replace db '((- 6.242869E-9)) :end 0) (replace db '(3.423897E-7) :end 0)
  (replace db '((- 2.5804031E-6)) :end 0)
  (replace db '((- 8.423285E-5)) :end 0) (replace db '(0.0044421284) :end 0)
  (replace db '((- 0.27757135)) :end 0) (if (< x 0.0) (go label300))
  (if (> c 5.0) (go label200)) (if (> x 1.2) (go label150))
  (setf t_ (* (+ (+ x x) (- 1.2)) con4)) (setf tt (+ t_ t_)) (setf j n1)
  (setf f1 (fref ak1 j)) (setf f2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m1) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref ak1 j))) (setf f2 temp1)
  ))
  (setf ai (+ (+ (* t_ f1) (- f2)) (fref ak1 1))) (setf j n1d)
  (setf f1 (fref dak1 j)) (setf f2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m1d) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref dak1 j))) (setf f2 temp1)
  ))
  (setf dai (- (+ (+ (* t_ f1) (- f2)) (fref dak1 1)))) (go end_label) label150
  (setf t_ (* (+ (+ x x) (- con2)) con3)) (setf tt (+ t_ t_)) (setf j n2)
  (setf f1 (fref ak2 j)) (setf f2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m2) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref ak2 j))) (setf f2 temp1)
  ))
  (setf rtrx (sqrt rx)) (setf ec (exp (- c)))
  (setf ai (/ (* ec (+ (+ (* t_ f1) (- f2)) (fref ak2 1))) rtrx)) (setf j n2d)
  (setf f1 (fref dak2 j)) (setf f2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m2d) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref dak2 j))) (setf f2 temp1)
  ))
  (setf dai (* (* (* -1 ec) (+ (+ (* t_ f1) (- f2)) (fref dak2 1))) rtrx))
  (go end_label) label200 (setf t_ (+ (/ 10.0 c) (- 1.0))) (setf tt (+ t_ t_))
  (setf j n1) (setf f1 (fref ak3 j)) (setf f2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m1) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref ak3 j))) (setf f2 temp1)
  ))
  (setf rtrx (sqrt rx)) (setf ec (exp (- c)))
  (setf ai (/ (* ec (+ (+ (* t_ f1) (- f2)) (fref ak3 1))) rtrx)) (setf j n1d)
  (setf f1 (fref dak3 j)) (setf f2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m1d) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref dak3 j))) (setf f2 temp1)
  ))
  (setf dai (* (* (* -1 rtrx) ec) (+ (+ (* t_ f1) (- f2)) (fref dak3 1))))
  (go end_label) label300 (if (> c 5.0) (go label350))
  (setf t_ (+ (* 0.4 c) (- 1.0))) (setf tt (+ t_ t_)) (setf j n3)
  (setf f1 (fref ajp j)) (setf e1 (fref ajn j)) (setf f2 0.0) (setf e2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m3) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1) (setf temp2 e1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref ajp j)))
    (setf e1 (+ (+ (* tt e1) (- e2)) (fref ajn j))) (setf f2 temp1)
    (setf e2 temp2)
  ))
  (setf ai
   (+ (+ (+ (* t_ e1) (- e2)) (fref ajn 1))
    (* (* -1 x) (+ (+ (* t_ f1) (- f2)) (fref ajp 1)))
  ))
  (setf j n3d) (setf f1 (fref dajp j)) (setf e1 (fref dajn j)) (setf f2 0.0)
  (setf e2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m3d) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1) (setf temp2 e1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref dajp j)))
    (setf e1 (+ (+ (* tt e1) (- e2)) (fref dajn j))) (setf f2 temp1)
    (setf e2 temp2)
  ))
  (setf dai
   (+ (* (* x x) (+ (+ (* t_ f1) (- f2)) (fref dajp 1)))
    (+ (+ (* t_ e1) (- e2)) (fref dajn 1))
  ))
  (go end_label) label350 (setf t_ (+ (/ 10.0 c) (- 1.0))) (setf tt (+ t_ t_))
  (setf j n4) (setf f1 (fref a j)) (setf e1 (fref b j)) (setf f2 0.0)
  (setf e2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m4) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1) (setf temp2 e1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref a j)))
    (setf e1 (+ (+ (* tt e1) (- e2)) (fref b j))) (setf f2 temp1)
    (setf e2 temp2)
  ))
  (setf temp1 (+ (+ (* t_ f1) (- f2)) (fref a 1)))
  (setf temp2 (+ (+ (* t_ e1) (- e2)) (fref b 1))) (setf rtrx (sqrt rx))
  (setf cv (+ c (- fpi12))) (setf ccv (cos cv)) (setf scv (sin cv))
  (setf ai (/ (+ (* temp1 ccv) (* (* -1 temp2) scv)) rtrx)) (setf j n4d)
  (setf f1 (fref da j)) (setf e1 (fref db j)) (setf f2 0.0) (setf e2 0.0)
  (fdo ((i 1 (+ i 1))) ((> i m4d) nil)
   (tagbody (setf j (+ j (- 1))) (setf temp1 f1) (setf temp2 e1)
    (setf f1 (+ (+ (* tt f1) (- f2)) (fref da j)))
    (setf e1 (+ (+ (* tt e1) (- e2)) (fref db j))) (setf f2 temp1)
    (setf e2 temp2)
  ))
  (setf temp1 (+ (+ (* t_ f1) (- f2)) (fref da 1)))
  (setf temp2 (+ (+ (* t_ e1) (- e2)) (fref db 1)))
  (setf e1 (+ (* ccv con5) (* 0.5 scv)))
  (setf e2 (+ (* scv con5) (* (* -1 0.5) ccv)))
  (setf dai (* (+ (* temp1 e1) (* (* -1 temp2) e2)) rtrx)) (go end_label)
  end_label (return (values x rx c ai dai))
))

