;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SDT-Tool
;; 
;; Copyright (C) 1999. William L. Oliver
;; 
;; This program is distributed as freeware without any warranty.  The program 
;; has been tested, but you must use it at your own risk.  For details see the 
;; GNU Public License, which is available by writing to:
;; 
;;   Free Software Foundation, Inc.
;;   675 Mass Ave.
;;   Cambridge, MA 02139, USA
;;   
;; Send questions and comments to Bill Oliver
;;      william.oliver@colorado.edu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes:
;; 
;; Signal detection theory is closely related to classical statistical
;; decision theory developed by Neyman, Pearson, and Wald. See the references
;; below for theory and applications.
;; 
;; To use the progam, open xlisp-stat and load this file (SDT-Tool.lsp). A
;; menu item with the name "Signal Detection" will appear. The menu options are:
;; 
;; ANALYZE 2X2: Enter values into a 2X2 table. The tab key may be used to move from
;; one field to the next. Click on the "Analyze" button when you have entered your
;; data. 
;; 
;; RATING ANALYSIS: When this menu is selected you are prompted for the number of
;; rating categories. Currently, this value may not exceed 12.
;; 
;; EXPLORE NORMAL MODEL: Use the sliders to change various parameters of the
;; Signal Detection Model. If you click on the graphs, the coordinates of your
;; mouse position will appear in the left hand corner of the window--this is
;; useful for table look-up. You may change the mouse mode so that when you
;; click-and-hold on a location for the Noise+Signal plot you will see the value
;; of Beta and various side effects. I'm especially fond of the latter feature.
;; 
;; CONVERT VALUES: Da or dprime values may be converted to Az, and vice versa.
;; Simply enter the values in the boxes and press the appropriate arrows. The
;; other conversions are of dubious value.

;; Hand, D. J. (1997). Construction and assessment of classification rules. 
;; New York: Wiley.
;; 
;; Kraemer, H. C. (1988). Assessment of 2X2 associations: Generalizations of
;; signal-detection methodology. American Statistician, 42, 37-49.
;; 
;; Macmillan, N. A., Creelman, C. D. (1991). Detection theory: A User's Guide.
;; New York: Cambridge University Press.
;; 
;; Swets, J. A. (1996). Signal detection theory and ROC analysis in psychology
;; and diagnostics. Mahwah, NJ: Erlbaum.
;; 
;; Known Problems:
;; 
;; I haven't figured out a good way to deal with the various screen resolutions.
;; The ideal resolution is 1024X768 for a Mac display. The windows are not
;; well-aligned at higher resolutions on a Mac. For non-Mac displays, the
;; various windows are tiled and will need to be moved about.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General prototype methods for plotting densities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto funplot-proto '(title funs colors parms domain) ()
                          scatterplot-proto)

(defmeth funplot-proto :isnew (&key title funs colors parms domain)
  (setf (slot-value 'title) title)
  (setf (slot-value 'funs) funs)
  (setf (slot-value 'colors) colors)
  (setf (slot-value 'parms) parms)
  (setf (slot-value 'domain) domain)
  (call-next-method 2 :show nil ) )
                        
;accessor methods

(defmacro normal-accessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (if set (setf (slot-value ',slot) content))
     (slot-value ',slot)) )

(defmacro replot-accessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (cond  (set (setf (slot-value ',slot) content)
               (send self :replot))
          (t nil))
   (slot-value ',slot)) )

(defmacro rescale-accessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (cond  (set (setf (slot-value ',slot) content)
               (send self :replot)
               (send self :adjust-to-data))
          (t nil))
   (slot-value ',slot)) )

(normal-accessor :title title funplot-proto)

(normal-accessor :funs funs funplot-proto)

(normal-accessor :colors colors funplot-proto)

(replot-accessor :parms parms funplot-proto)

(rescale-accessor :domain domain funplot-proto)

;plotting methods

(defmeth funplot-proto :replot ()
  (let ((clist (list (send self :parms) (send self :funs)
                     (send self :colors)))
        (nplots (length (send self :funs)))
        (function nil)
        (p1 nil)
        (p2 nil)
        (cs nil)
        (d1 (first (send self :domain))) 
        (d2 (second (send self :domain))) )   
    (if (/= 1 (length (remove-duplicates (mapcar #'length clist))))
      (error "Unequal numbers of functions, colors, param lists!"))
    (send self :clear)    
    (dotimes (i nplots)
      (setf function (select (send self :funs) i))
      (setf p1 (first (select (send self :parms) i)))
      (setf p2 (second (select (send self :parms) i)))
      (setf cs (select (send self :colors) i))    	      
      (send self :add-function  #'(lambda (x) 
          (funcall function x p1 p2)) d1 d2 :color cs) )
    (send self :redraw)))
   
(defmeth funplot-proto :rescale ()
  (send self :adjust-to-data) )

(defmeth funplot-proto :shade (nfun xval1 xval2 &optional (nlines 35) (offset 0))
  (let* ((xvals (rseq (+ offset xval1) xval2 nlines))
      (function (select (send self :funs) nfun))
      (p1 (first (select (send self :parms) nfun)))
      (p2 (second (select (send self :parms) nfun)))
      (cs (select (send self :colors) nfun))
      (yvals (mapcar #'(lambda (x) (funcall function x p1 p2)) xvals)))
    (dotimes (i nlines)
      (send self :add-lines 
        (list (select xvals i) (select xvals i))
        (list 0 (select yvals i))
        :type 'solid :color cs) ) ) )

(defmeth funplot-proto :draw-line-coord (nfun xval &optional (c1 'black) (c2 'black))
  (let* ((function (select (send self :funs) nfun))
         (p1 (first (select (send self :parms) nfun)))
         (p2 (second (select (send self :parms) nfun)))
         (yval (funcall function xval p1 p2)))
     (send self :add-lines (list xval xval) (list 0 yval) :type 'solid :color c1)
     (send self :add-lines (list 0 xval) (list yval yval) :type 'solid :color c2)))

 
;; Code adapted from Tierney for showing coordinates of a mouse click
 
(send funplot-proto :add-mouse-mode 'show-coords
        :title "Show Co-ordinates"
        :click :do-show-coordinates
        :cursor 'finger)

(defmeth funplot-proto :do-show-coordinates (x y m1 m2)
  (let* ((xy (send self :canvas-to-real x y))
    (s (format nil "(~5,3f ~5,3f)" (first xy) (second xy)))
    (mode (send self :draw-mode)))
  (send self :draw-mode 'xor)
  (send self :draw-string s 10 10)
  (send self :while-button-down #'(lambda (x y ) nil))
  (send self :draw-string s 10 10)
  (send self :draw-mode mode)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specialized Prototypes for Signal Detection Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun activep (x)
  (member x (active-windows)))

(defproto signal-proto '(title sf nf
    h-rate fa-rate  zfa-rate zh-rate
    init-chisq chisq expected observed
    n2 roc zroc
    init-a init-b a b s2 var-covar
    d1 d2 da az
    d1-var d2-var da-var az-var
    criteria c1 c2 c beta
    criteria-var c1-var c2-var c-var) )
                   
(normal-accessor :title title signal-proto)
(normal-accessor :sf sf signal-proto)
(normal-accessor :nf nf signal-proto)

(normal-accessor :h-rate h-rate signal-proto)
(normal-accessor :fa-rate fa-rate signal-proto)
(normal-accessor :zfa-rate zfa-rate signal-proto)
(normal-accessor :zh-rate zh-rate signal-proto)

(normal-accessor :init-chisq init-chisq signal-proto)
(normal-accessor :chisq chisq signal-proto)
(normal-accessor :expected expected signal-proto)
(normal-accessor :observed observed signal-proto)

(normal-accessor :n2 n2 signal-proto)
(normal-accessor :roc roc signal-proto)
(normal-accessor :zroc zroc signal-proto)

(normal-accessor :init-a init-a signal-proto)
(normal-accessor :init-b init-b signal-proto)
(normal-accessor :a a signal-proto)
(normal-accessor :b b signal-proto)
(normal-accessor :s2 s2 signal-proto)
(normal-accessor :var-covar var-covar signal-proto)

(normal-accessor :d1 d1 signal-proto)
(normal-accessor :d2 d2 signal-proto)
(normal-accessor :da da signal-proto)
(normal-accessor :az az signal-proto)

(normal-accessor :d1-var d1-var signal-proto)
(normal-accessor :d2-var d2-var signal-proto)
(normal-accessor :da-var da-var signal-proto)
(normal-accessor :az-var az-var signal-proto)

(normal-accessor :criteria criteria signal-proto)
(normal-accessor :c1 c1 signal-proto)
(normal-accessor :c2 c2 signal-proto)
(normal-accessor :c c signal-proto)
(normal-accessor :beta beta signal-proto)

(normal-accessor :criteria-var criteria-var signal-proto)
(normal-accessor :c1-var c1-var signal-proto)
(normal-accessor :c2-var c2-var signal-proto)
(normal-accessor :c-var c-var signal-proto)

(defmeth signal-proto :isnew (sf nf &key (title "Signal Detection"))
  (send self :title title)
  (send self :sf sf)
  (send self :nf nf)
  (send self :d1 nil)
  (send self :s2 nil)
  (send self :n2 nil)
  (send self :roc nil)
  (send self :zroc nil)
  (if title (send self :title title)))

(defmeth signal-proto :init-n2 ()
  (let ((n2x (send funplot-proto :new
          :title (send self :title "Noise+Signal")
          :funs '(normal-dens2 normal-dens2)
          :colors '(green red)
          :parms  '((1 1) (0 1))
          :domain '(-5 10) )))
    (send n2x  :variable-label 0 "x = Familiarity (in units of z)")
    (send n2x :variable-label 1 "f(x|S_n)")
    (send n2x :showing-labels)
    (send n2x :redraw)
    (send n2x :delete-mouse-mode 'selecting)
    (send n2x :delete-mouse-mode 'brushing)
    (send n2x :mouse-mode 'show-beta)
    (send n2x :mouse-mode 'show-coords)
    (send n2x :show-window)
    (send n2x :replot)
    (send n2x :rescale)
    (send self :n2 n2x) ))

(defmeth signal-proto :init-roc ()
  (let ((rocx (send funplot-proto :new
          :title "ROC Curve"
          :funs '(hr-d linear)
          :colors '(blue black)
          :parms  '((1 1) (0 1))
          :domain '(.0001 .9999) )))
    (send rocx :variable-label 0 "False Alarm Rate (F)")
    (send rocx :variable-label 1 "Hit Rate (H)")
    (send rocx :showing-labels)
    (send rocx :mouse-mode 'show-coords)
    (send rocx :show-window)
    (send rocx :replot)
    (send rocx :rescale)
    (send self :roc rocx)))

(defmeth signal-proto :init-zroc ()
  (let ((zrocx (send funplot-proto :new
          :title "z-ROC Curve"
          :funs '(zroc-fun linear)
          :colors '(blue black)
          :parms  '((1 1) (0 1))
          :domain '(-5 5) )))
    (send zrocx :variable-label 0 "z(F)")
    (send zrocx :variable-label 1 "z(H)")
    (send zrocx :showing-labels)
    (send zrocx :mouse-mode 'show-coords)
    (send zrocx :show-window)
    (send zrocx :replot)
    (send zrocx :rescale)
    (send self :zroc zrocx)))

(defmeth signal-proto :update-n2 (&key (crit-2 nil) (crit-rate nil))
  (let ((c1 (send self :c1))
        (d1 (send self :d1))
        (s2 (send self :s2))
        (n2x (send self :n2)))
  (when (member n2x (active-windows))
    (send n2x :parms (list (list d1 s2) (list 0 1)))
    (when crit-2
      (let* ((c1 (send self :c1))
            (x (c1-to-x c1 d1 s2)))
      (send n2x :add-lines (list x x) (list 0 0.4) :type 'solid)
      (send n2x :shade 0 x 10 50 .1)
      (send n2x :shade 1 x 10 50 .05)))
    (when crit-rate
      (mapcar #'(lambda (x) (send n2x :add-lines 
            (list x x) (list 0 0.4) :type 'solid)) 
            (- (reverse (send self :criteria))))))))

(defmeth signal-proto :update-roc (&key (crit-2 nil))
   (let ((d1 (send self :d1))
         (s2 (send self :s2))
         (rocx (send self :roc)))
  (when (member rocx (active-windows))
      (send rocx :parms (list (list d1 s2) (list 0 1)))
      (when crit-2
        (let* ((c1 (send self :c1))
               (x (c1-to-x c1 d1 s2))
               (fa (- 1 (normal-cdf2 x 0 1))))     
          (send rocx :draw-line-coord 0 fa 'red 'green))))))
  
(defmeth signal-proto :update-zroc (&key (crit-2 nil))
  (let ((d1 (send self :d1))
        (s2 (send self :s2))
        (zrocx (send self :zroc)))
  (when (member zrocx (active-windows))
      (send zrocx :parms (list (list d1 s2) (list 0 1)))
      (when crit-2
        (let* ((c1 (send self :c1))
               (x (c1-to-x c1 d1 s2))
               (fa (- 1 (normal-cdf2 x 0 1)))
               (z-fa (normal-quant2 fa 0 1))       
               (z-hr (zroc-fun z-fa d1 s2)))         
          (send zrocx :add-lines '(0 0) '(-5 10) :type 'dashed)
          (send zrocx :add-lines '(-6 6) '(0 0) :type 'dashed) 
          (send zrocx :add-points (list z-fa) (list z-hr)))))))

(defmeth signal-proto :analyze-2by (&key (print nil) (plot nil))
  (let* ((sf (send self :sf))
         (nf (send self :nf))
         (a (first sf))
         (b (second sf))
         (c (first nf))
         (d (second nf))
         (hr (hit-rate-2by a b))
         (fa (fa-rate-2by c d))
         (dpx (dprime-2by a b c d))
         (dp (first dpx))
         (dpvar (second dpx))
         (cx (c-2by fa hr))
         (cxvar (* 0.25 dpvar))
         (beta (beta-2by fa hr)))
   (send self :h-rate hr)
   (send self :fa-rate fa)
   (send self :da dp)
   (send self :da-var dpvar)
   (send self :c cx)
   (send self :c-var cxvar)
   (send self :beta beta)
   (if print (send self :print-2by))
   (send self :d1 dp)
   (send self :s2 1.0)
   (send self :c1 cx)
   (when plot
     (send self :update-n2 :crit-2 t)
     (send self :update-roc :crit-2 t)
     (send self :update-zroc :crit-2 t))))

(defmeth signal-proto :print-2by ()
  (let* ((sf (send self :sf))
         (nf (send self :nf))
         (a (first sf))
         (b (second sf))
         (c (first nf))
         (d (second nf))
         (hr (send self :h-rate))
         (fa (send self :fa-rate))
         (dp (send self :da))
         (dpsee (sqrt (send self :da-var)))
         (cx (send self :c))
         (cxsee (sqrt (send self :c-var)))
         (beta (send self :beta))
         (log-beta (log beta)))
  (terpri)
  (format t "~%    Signal Detection Analysis of 2X2 Frequency Table~%")
  (format t "-----------------------------------------------------------~%")
  (format t "         Hits: ~5d                  Misses: ~5d~%" a b)
  (format t " False Alarms: ~5d      Correct Rejections: ~5d~%" c d)
  (terpri)
  (format t "     Hit Rate: ~7,3f      False Alarm Rate: ~7,3f~%" hr fa)
  (terpri)
  (format t "           d': ~7,3f                     c: ~7,3f~%" dp cx)
  (format t "       SEE d': ~7,3f                 SEE c: ~7,3f~%" dpsee cxsee)
  (terpri)
  (format t "         beta: ~7,3f              log beta: ~7,3f~%" beta log-beta)
  (format t "-----------------------------------------------------------~%")
  (terpri)
  (when (< (sum (list a b c d)) 60)
    (format t "WARNING STANDARD ERRORS NOT ASYMTOTICALLY ACCURATE BECAUSE OF LOW N~%"))))

;;Some duplication with analyze-rate, but this may be useful later

(defmeth signal-proto :plot-roc-points ()
  (let ((sf (send self :sf))
        (nf (send self :nf))
        (roc (send self :roc))
        (zroc (send self :zroc)))
    (when (and sf nf)
      (send self :fa-rate (cumsum (/ nf (sum nf))))
      (send self :h-rate (cumsum (/ sf (sum sf))))
      (send self :zfa-rate (normal-quant (butlast (send self :fa-rate))))
      (send self :zh-rate (normal-quant (butlast (send self :h-rate))))        
      (when (activep roc)
        (send roc :add-points (send self :fa-rate) (send self :h-rate)))
      (when (activep zroc)
        (send zroc :add-points (send self :zfa-rate) (send self :zh-rate))))))

(defmeth signal-proto :fit-model (&key (verbose t))
  (let* ((nf (send self :nf))
        (sf (send self :sf)) (ex nil) (obs nil)     
        (m nil) (theta nil) (params nil))
    (when (and sf nf)
      (send self :fa-rate (cumsum (/ nf (sum nf))))
      (send self :h-rate (cumsum (/ sf (sum sf))))
      (send self :zfa-rate (normal-quant (butlast (send self :fa-rate))))
      (send self :zh-rate (normal-quant (butlast (send self :h-rate))))
      (setf m (regression-model (send self :zfa-rate)
                (send self :zh-rate) :print nil))
      (send self :init-a (- (first (send m :coef-estimates))))
      (send self :init-b  (second (send m :coef-estimates)))      
      (setf ex (combine (cddr
         (expected-freqs (- (send self :init-a)) (send self :init-b) nf sf 
           (send self :zfa-rate)))))
      (setf obs (combine nf sf))
      (send self :init-chisq (chi-square ex obs (- (length sf) 3)))
      (setf theta (combine (send self :init-a) (send self :init-b)
           (send self :zfa-rate)))
      (setf params (signal-max theta nf sf verbose))
      (send self :s2 (/ 1 (second params)))
      (send self :d1 (/ (- (first params)) (second params)))
      (send self :a (- (first params)))
      (send self :b (second params))
      (send self :var-covar (inverse 
          (- (numhess #'(lambda (x) (log-like-sig x nf sf)) params))))
      (send self :criteria (cddr params))
      (send self :criteria-var 
          (cddr (diagonal (send self :var-covar)))))))

(defmeth signal-proto :update-stats ()
  (let* ((nf (send self :nf))
        (sf (send self :sf))
        (d1 (send self :d1))
        (s2 (send self :s2))
        (crit (send self :criteria))
        (a 0) (b 0) (va 0) (vb 0) (vab 0))
    (when (send self :a)      
      (setf a (send self :a))
      (setf b (send self :b))
      (send self :expected (expected-freqs a b nf sf (send self :criteria)))
      (send self :observed (observed-freqs nf sf))
      (send self :chisq (chi-square (combine (cddr (send self :expected)))
                                    (combine nf sf) (- (length sf) 3)))
      (setf va (aref (send self :var-covar) 0 0))
      (setf vb (aref (send self :var-covar) 1 1))
      (setf vab (- (aref (send self :var-covar) 0 1)))
      (send self :da (ab-to-da a b))
      (send self :da-var (var-2param #'ab-to-da a b va vb vab))
      (send self :az (ab-to-az a b))
      (send self :az-var (var-2param #'ab-to-az a b va vb vab))
      (send self :c1 (mapcar #'(lambda (x) (zk-to-c1 x d1 s2)) 
          (- (reverse crit))))
      (send self :c1-var (reverse (send self :criteria-var)))
      (send self :beta (mapcar #'(lambda (x) (z-to-beta x d1 s2)) 
          (- (reverse crit)))))))

(defmeth signal-proto :print-model-fit ()
    (let* ((init-ch (send self :init-chisq))
         (init-cs (first init-ch))
         (init-df (second init-ch))
         (init-p (third init-ch))
         (ch (send self :chisq))
         (cs (first ch))
         (df (second ch))
         (p (third ch))
         (a (send self :a))
         (b (send self :b))
         (ab (diagonal (send self :var-covar)))
         (sa (sqrt (first ab)))
         (sb (sqrt (second ab)))
         (ex-n (combine (third (send self :expected))))
         (ex-sn (combine (fourth (send self :expected)))))
     (format t "MODEL FIT:~%")
     (format t "Observed Response Frequencies:~%")
     (format t "s+n: ")
     (mapcar #'(lambda (x) (format t "~5d " x)) (send self :sf))
     (format t "~%  n: ")
     (mapcar #'(lambda (x) (format t "~5d " x)) (send self :nf))
     (format t "~%~%Predicted Response Frequencies:~%")
     (format t "s+n: ")
     (mapcar #'(lambda (x) (format t "~8,2f " x)) ex-sn)
     (format t "~%  n: ")
     (mapcar #'(lambda (x) (format t "~8,2f " x)) ex-n)
     (format t "~%~%Initial Chi Square: ~,3f    df: ~d    p: ~,3f~%" init-cs 
       init-df init-p)
     (format t "  Final Chi Square: ~,3f    df: ~d    p: ~,3f~%" cs df p)
     (format t "~%a: ~,3f (~,3f)     b: ~,3f (~,3f)~%~%"  a sa b sb)))       
         
(defmeth signal-proto :print-sensitivity ()
   (let ((d1 (send self :d1))
         (s2 (send self :s2))
         (da (send self :da))
         (da-se (sqrt (send self :da-var)))
         (az (send self :az))
         (az-se (sqrt (send self :az-var))))
  (format t "SENSITIVITY:~%") 
  (format t "d(a): ~,3f (~,3f)    A(z): ~,3f (~,3f)~%~%" da da-se az az-se)))   

(defmeth signal-proto :print-bias ()
  (let ((crit (- (reverse (send self :criteria))))
        (crit-se (sqrt (reverse (send self :criteria-var))))
        (beta (send self :beta))
        (c1 (send self :c1))
        (c1-se (sqrt (send self :c1-var))))
    (format t "RESPONSE BIAS:")
    (format t "~%-Z(k): ")
    (mapcar #'(lambda (x y) (format t "~,3f (~,3f) " x y)) crit crit-se)
    (format t "~%   c1: ")
    (mapcar #'(lambda (x y) (format t "~,3f (~,3f) " x y)) c1 c1-se)
    (format t "~%~%    beta: ")
    (mapcar #'(lambda (x) (format t "~8,3f " x)) beta)
    (format t "~%log beta: ")
    (mapcar #'(lambda (x) (format t "~8,3f " x)) (log beta))
    ))

(defmeth signal-proto :print-rate ()
  (format t "~%~%         Signal Detection Analysis of Rating Data~%")
  (format t "-----------------------------------------------------------~%")
  (send self :print-model-fit)
  (send self :print-sensitivity)
  (send self :print-bias)
  (format t "~%-----------------------------------------------------------~%")
  (format t "Note: Standard Errors appear in parentheses~%~%"))

(defmeth signal-proto :print-roc-stats ()
  (let* ((d1 (send self :d1))
         (s2 (send self :s2))
         (c1 (send self :c1))
         (s  (/ 1 (send self :s2)))
         (a (* d1 s))
         (da (d1-to-da d1 s))
         (az (da-to-az da))
         (c (c1-to-x c1 d1 s2))
         (beta (beta-val c d1 s2)))
   (format t "~%~%                     Parameter Values~%")
   (format t "-----------------------------------------------------------~%")    
   (format t "MODEL PARAMETERS:~%")
   (format t "a: ~,3f    b: ~,3f~%~%" a s)
   (format t "SENSITIVITY:~%") 
   (format t "d(a): ~,3f    d1: ~,3f    A(z): ~,3f    ~%~%" da d1 az)
   (format t "RESPONSE BIAS:~%")
   (format t "c1: ~,3f    beta: ~,3f    log beta: ~,3f~%" c1 beta (log beta))
   (format t "-----------------------------------------------------------~%")))
   
(defmeth signal-proto :analyze-rate ()
  (let ((roc (send self :roc))
        (zroc (send self :zroc)))
      (send self :fit-model)
      (send self :update-stats)
      (send self :update-n2 :crit-rate t)
      (send self :update-roc)
      (send self :update-zroc)
      (send self :print-rate)
      (when (activep roc)
        (send roc :add-points (send self :fa-rate) (send self :h-rate)))
      (when (activep zroc)
        (send zroc :add-points (send self :zfa-rate) 
          (send self :zh-rate)))))

(defmeth signal-proto :dispose-plots ()
  (let ((n2 (send self :n2))
        (roc (send self :roc))
        (zroc (send self :zroc)))
  (when (activep n2)
    (send n2 :dispose))
  (when (activep roc)
    (send roc :dispose))
  (when (activep zroc)
    (send zroc :dispose))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Normal distribution (Adapted from code at UCLA web site)

(defun normal-dens2 (x &optional (mu 0) (sigma 1))
  (/ (normal-dens (/ (- x mu) sigma)) sigma))

(defun normal-cdf2 (x &optional (mu 0) (sigma 1))
  (normal-cdf (/ (- x mu) sigma)))
 
(defun normal-quant2 (p &optional (mu 0) (sigma 1))
  (+ mu (* sigma (normal-quant p))))

;; Numerical calculations, replace many later with local functions (flets)

(defun hr-d (fa dp s2)
  (normal-cdf2 (+ (normal-quant fa) dp) 0 s2))

(defun linear (x intercept slope)
  (+ intercept (* x slope)) )

(defun zroc-fun (x d1 s2)
  (let* ((s (/ 1 s2))
        (d2 (* d1 s)))
  (+ d2 (* x s))))

(defun c1-to-x (c1 d1 s2)
  (+ c1 (/ d1 (+ 1 s2))))

(defun zk-to-c1 (zk d1 s2)
  (- zk (/ d1 (+ 1 s2))))

(defun beta-val (x d1 s2)
  (/ (normal-dens2 x d1 s2) (normal-dens2 x 0 1)))

(defun da-to-d1 (da s)
  (/ (* da (sqrt (* .5 (+ 1 (* s s))))) s))

(defun da-to-az (da)
  (normal-cdf (/ da (sqrt 2))))

(defun az-to-da (az)
 (* (normal-quant az) (sqrt 2)))

(defun d1-to-da (d1 s)
  (/ (* s d1) (sqrt (* .5 (+ 1 (* s s))))))

(defun ab-to-da (a b)
  (let ((b21 (+ (* b b) 1)))
    (/ (* a (sqrt 2)) (sqrt b21))))
        
(defun ab-to-az (a b)
  (let ((v (/ a (sqrt (+ (^ b 2) 1)))))
    (normal-cdf v)))

(defun var-2param (f a b va vb vab)
  "variance of composite parameter via linearization of gradient"
  (let* ((d1 (* 3 (sqrt va)))
         (d2 (* 3 (sqrt vb)))
         (y11 (funcall f (- a d1) b))
         (y12 (funcall f (+ a d1) b))
         (y21 (funcall f a (- b d2)))
         (y22 (funcall f a (+ b d2)))
         (grad1 (/ (- y12 y11) (* 2 d1)))
         (grad2 (/ (- y22 y21) (* 2 d2))))
   (+ (* grad1 grad1 va) (* grad2 grad2 vb) (* 2 grad1 grad2 vab))))

;; functions for 2X2 Analysis
(defun dprime-2by (a b c d)
  "args: a b c d  Returns dprime and variance of dprime as list"
  (let* ((hr (/ a (+ a b)))
      (fa (/ c (+ c d)))
      (dprime (- (normal-quant hr) (normal-quant fa)))
      (num1 (* hr (- 1 hr)))
      (denom1 (* (+ a b) (^ (normal-dens (normal-quant hr)) 2)))
      (num2 (* fa (- 1 fa)))
      (denom2 (* (+ c d) (^ (normal-dens (normal-quant fa)) 2)))
      (d-var (+ (/ num1 denom1) (/ num2 denom2))))
    (list dprime d-var) ))
   
(defun hit-rate-2by (a b)
  (/ a (+ a b)))

(defun fa-rate-2by (c d)
  (/ c (+ c d)))

(defun beta-2by (fa hr)
  (let ((zfa (normal-quant fa))
        (zhr (normal-quant hr)))
      (/ (normal-dens zhr) (normal-dens zfa))))

(defun c-2by (fa hr)
  (let ((zfa (normal-quant fa))
        (zhr (normal-quant hr)))
      (* -0.5 (+ zhr zfa))))
  
(defun da-2by (fa hr s)
   (let* ((zfa (normal-quant fa))
        (zhr (normal-quant hr))
        (term1 (sqrt (/ 2 (+ 1 (* s s)))))
        (term2 (- zhr (* s zfa))))
      (* term1 term2) ) )
  
(defun c1-2by (fa hr s)
   (let* ((zfa (normal-quant fa))
        (zhr (normal-quant hr))
        (term1 (/ -1 (+ 1 s)))
        (term2 (+ zhr zfa)))
      (* term1 term2)))
 
(defun z-to-beta (z d1 s2)
  (/ (normal-dens2 z d1 s2) (normal-dens2 z 0 1)))
  
(defun log-like-sig (theta f h)
  (let* ((a (select theta 0))
        (b (select theta 1))
        (z (cddr theta))
        (p1 (difference (combine 0 (normal-cdf z) 1)))
        (p2 (difference (combine 0 (normal-cdf (- (* b z) a)) 1))))
   (sum (* (log p1) f) (* (log p2) h))))

(defun signal-max (th f h verb)
  (newtonmax #'(lambda (x) (log-like-sig x f h)) th :verbose verb))

(defun expected-freqs (a b f h crit)
  (let ((p1 (difference (combine 0 (normal-cdf crit) 1)))
        (p2 (difference (combine 0 (normal-cdf (+ (* b crit) a)) 1))))
  (list p1 p2 (* p1 (sum f)) (* p2 (sum h))) ))

(defun observed-freqs (f h)
  (let ((p1 (/ f (sum f)))
        (p2 (/ h (sum h))))
    (list p1 p2 f h)))
      
(defun chi-square (ex ob df)
  (let ((c (sum (/ (^ (- ex ob) 2) ex))))
    (cond ((= df 0) (combine nil nil nil))
          (t (combine c df (- 1 (chisq-cdf c df)))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(export 'xlisp::active-windows 'xlisp)
(defparameter *float-format* "%g")

(format t "~%~%SDT-Tool, Version 0.6~%~%")
(format t "Copyright (C) 1998. William L. Oliver~%")
(format t "This program is distributed as freeware without ~%")
(format t "any warranty. The program has been tested, but you ~%")
(format t "must use it at your own risk. For details see the ~%")
(format t "GNU Public License, which is available by writing to: ~%")
(format t "Free Software Foundation, Inc., 675 Mass Ave., ~%")
(format t "Cambridge, MA 02139, USA.~%~%")
(format t "Send questions and comments to Bill Oliver~%")
(format t "william.oliver@colorado.edu~%~%")

;; Add additional mouse mode to show the value of beta

(defun draw-beta-events (x)
  (let* ((n2 (send *sd-mod* :n2))
         (roc (send *sd-mod* :roc))
         (zroc (send *sd-mod* :zroc))
         (d1 (send *sd-mod* :d1))
         (s2 (send *sd-mod* :s2))
         (fa (- 1 (normal-cdf2 x 0 1)))
         (z-fa (normal-quant2 fa 0 1))
         (z-hr (zroc-fun z-fa d1 s2)))
    (send n2 :add-lines (list x x) (list 0 0.4) :type 'solid)
    (send n2 :shade 0 x 10 50 .1)
    (send n2 :shade 1 x 10 50 .05)
    (when (activep roc)
      (send roc :draw-line-coord 0 fa 'red 'green))
    (when (activep zroc)
      (send zroc :add-points (list z-fa) (list z-hr)))))

(send funplot-proto :add-mouse-mode 'show-beta
        :title "Show-Beta-at-click"
        :click :do-show-beta
        :cursor 'finger)

(defmeth funplot-proto :do-show-beta (x y m1 m2)
  (let* ((xy (send self :canvas-to-scaled x y))
         (xb (first xy))
         (d1 (send *sd-mod* :d1))
         (s2 (send *sd-mod* :s2))
         (s (format nil "Beta= ~s" 
             (/ (normal-dens2 xb d1 s2) (normal-dens2 xb 0 1))))
         (mode (send self :draw-mode))
         (roc (send *sd-mod* :roc))
         (zroc (send *sd-mod* :zroc)))
  (when (activep roc)
    (send roc :replot))
  (when (activep zroc)
    (send zroc :clear-points))
  (send self :replot)
  (send self :draw-mode 'normal)
  (draw-beta-events xb)
  (send self :draw-mode 'xor)
  (send self :draw-string s x y)
  (send self :while-button-down #'(lambda (x y ) nil))
  (send self :draw-string s x y)
  (send self :draw-mode mode)))

;; Screen layout functions. The canonical layout is 1024X768 for
;; a Mac monitor because that's what I use most of the time.
;; The layout is a bit off when the Mac resolution is set to 1152X870
;; or higher. I will fix this some day. The windows are tiled for
;; Windoze and Unix displays.

#+macintosh(defun set-locs ()
  (when (and (boundp '*sd-mod*) (objectp *sd-mod*))
    (let* ((n2 (send *sd-mod* :n2))
          (roc (send *sd-mod* :roc))
          (zroc (send *sd-mod* :zroc))
          (x 0)
          (rloc (+ (send roc :location) 30))
          (zloc (+ (send zroc :location) 60)))
      (send n2 :size 590 212)
      (cond ((>= (prod (screen-size)) 700000)
          (send roc :location 235 270)
          (send zroc :location 516 270)
          (send n2 :location 210 25)
          (when (boundp '*sdio*)
            (setf x (first (send *sdio* :location)))
            (send *sdio* :location x 557))
          (when (boundp '*twodio*)
            (setf x (first (send *twodio* :location)))
            (send *twodio* :location x 552))
          (when (boundp '*ratedio*)
            (setf x (first (send *ratedio* :location)))
            (send *ratedio* :location x 552))
          )
        (t  (send roc :location (first rloc) (second rloc))
            (send zroc :location (first zloc) (second zloc)))))))

#-macintosh(defun set-locs ()
  (when (and (boundp '*sd-mod*) (objectp *sd-mod*))
    (let* ((n2 (send *sd-mod* :n2))
          (roc (send *sd-mod* :roc))
          (zroc (send *sd-mod* :zroc))
          (rloc (+ (send roc :location) 30))
          (zloc (+ (send zroc :location) 60)))
    (send n2 :size 590 212)
    (send roc :location (first rloc) (second rloc))
    (send zroc :location (first zloc) (second zloc)))))

(defun active-plot-windows ()
  (flet ((keep-p (x)
       (or (equalp (send x :slot-value 'title) "Menu Bar")
           (eql (send x :slot-value 'proto-name) 'listener-proto))))
         (remove-if #'keep-p (active-windows))))
  
(defun remove-graphs ()
   (mapcar #'(lambda (x) (send x :dispose)) (active-plot-windows)))
   
(defun remove-sn ()
  (remove-graphs)
  (send *detect-menu* :remove) )

(defun da-scroll-action (da)
  (let ((s2 (send *sd-mod* :s2)))
    (send *sd-mod* :d1 (da-to-d1 da (/ 1 s2)))
    (send *sd-mod* :update-n2 :crit-2 t)
    (send *sd-mod* :update-roc :crit-2 t)
    (send *sd-mod* :update-zroc :crit-2 t)))

(defun s2-scroll-action (p)
  (send *sd-mod* :s2 (/ 1 p))
  (send *sd-mod* :update-n2 :crit-2 t)
  (send *sd-mod* :update-roc :crit-2 t)
  (send *sd-mod* :update-zroc :crit-2 t))

(defun c1-scroll-action (p)
 (send *sd-mod* :c1 p)
 (send *sd-mod* :update-n2 :crit-2 t)
 (send *sd-mod* :update-roc :crit-2 t)
 (send *sd-mod* :update-zroc :crit-2 t))

(defun setup-sn-all ()
  (def *sd-mod* (send signal-proto :new 
      '(1 2)
      '(3 4)   
      :title "Subject 1?"))
  (send *sd-mod* :init-n2)
  (send *sd-mod* :init-roc)
  (send *sd-mod* :init-zroc)
  (send *sd-mod* :d1 0)
  (send *sd-mod* :s2 1)
  (send *sd-mod* :c1 0))

(defun roc-help ()
  (format t "~%Add Later~%"))

(defun roc-stats ()
  (send *sd-mod* :print-roc-stats))

(defun set-mouse-mode ()
  (send (send *sd-mod* :n2) :choose-mouse-mode))
  
;; Custom dialog for manipulating dprime, s2, and c1

(defun setup-sn-dialog ()
 (let* ((foo1 (remove-graphs))
  (foo2 (setup-sn-all))
  (da-label (send text-item-proto :new "da =" :text-length 3 ))
  (s-label (send text-item-proto :new "s =" :text-length 3))
  (c1-label (send text-item-proto :new "c1 =" :text-length 3 ))
  (dp-value (send text-item-proto :new "" :text-length 3))
  (s-value (send text-item-proto :new "" :text-length 3))
  (c1-value (send text-item-proto :new "" :text-length 3))
  (sep1 (send text-item-proto :new
                "___________________________________"))
  (da-scroll (send interval-scroll-item-proto :new
                     '(0 3.26)
                     :points 30
                     :text-item dp-value
                     :action #'da-scroll-action ))
  (s-scroll (send sequence-scroll-item-proto :new
    '(0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00 1.05
      1.10 1.15 1.20)
                     :text-item s-value
                     :action #'s2-scroll-action ))
   (c1-scroll (send interval-scroll-item-proto :new
                     '(-2 2)
                     :points 17
                     :text-item c1-value
                     :action #'c1-scroll-action))
  (clear-button (send button-item-proto :new "Close Explore"
                     :action #'remove-graphs))
;;   (help-button (send button-item-proto :new "Help"
;;                      :action #'roc-help))
  (mouse-button (send button-item-proto :new "Mouse Mode"
                     :action #'set-mouse-mode))
  (stats-button (send button-item-proto :new "Stats"
                     :action #'roc-stats)))
  (def *sdio* (send dialog-proto :new (list 
                    (list da-label dp-value da-scroll)
                    (list s-label s-value s-scroll)
                    (list c1-label c1-value c1-scroll)
                     sep1
;;                     (list stats-button help-button mouse-button clear-button))))
                     (list stats-button mouse-button clear-button))))

  (set-locs)
  (send *sdio* :title "Distribution Parameters")
  (send da-scroll :value 0)
  (send s-scroll :value 10)
  (send c1-scroll :value 0)
  (send *sdio* :show-window)))

;; A dialog for a 2x2 signal detection analysis.
;; The user inputs the frequecies

(defun zero-val ()
    (message-dialog 
"You have entered a frequecy of 0. See Hautus, M. J. (1995). 
Corrections for extreme proportions and their biasing effects 
on estimated values of d'. Behavior Research Methods, 
Instruments, & Computers, 27, 46-51.")
(error "Only positive values permitted!") )

(defun read-number (str)
  (let ((val (read-from-string str)))
   (cond 
     ((or (not (numberp val)) (< val 0))
          (message-dialog "Not a valid number! Re-enter the values")
          (error "Not a valid number! Re-enter the values."))
     ((= val 0) (zero-val))
     (t val) )))

(defun analyze-two (af bf cf df)
  (let* ((a (read-number (send af :text)))
     (b (read-number (send bf :text)))
     (c (read-number (send cf :text)))
     (d (read-number (send df :text))))
     (send *sd-mod* :sf (list a b))
     (send *sd-mod* :nf (list c d))
   (send *sd-mod* :analyze-2by :print t :plot t)))

(defun setup-two ()
  (let* ((enter-label (send text-item-proto :new "Enter Frequencies:"))
      (blank-label (send text-item-proto :new "" :text-length 4))
      (s2-label (send text-item-proto :new "sig" :text-length 3))
      (s1-label (send text-item-proto :new "blk" :text-length 3))
      (yes-label (send text-item-proto :new "Yes" :text-length 4))
      (no-label (send text-item-proto :new "No" :text-length 2))
      (clear-button (send button-item-proto :new "Close 2X2"
                     :action #'remove-graphs))
      (sep1 (send text-item-proto :new
                "________________________"))
      (a-field (send edit-text-item-proto :new "20" :text-length 4))
      (b-field (send edit-text-item-proto :new "5" :text-length 4))
      (c-field (send edit-text-item-proto :new "10" :text-length 4))
      (d-field (send edit-text-item-proto :new "25" :text-length 4))
      (analyze-button (send button-item-proto :new "Analyze"
        :action #'(lambda () (analyze-two a-field b-field c-field d-field)))))
      (remove-graphs)
      (def *twodio* (send dialog-proto :new (list
            enter-label
             (list blank-label yes-label no-label)
             (list s2-label a-field b-field)
             (list s1-label c-field d-field)
             sep1
             (list analyze-button clear-button)))
      )
      (setup-sn-all)
      (set-locs)
      (send *twodio* :title "2X2 Experiment")
      (send *twodio* :show-window)))

;; rating scale analysis
 
(defun read-fields ()
  (let ((sf nil)
        (nf nil)
        (str nil))
  (dotimes (i *nrate*)
    (setf str (format nil "*a~d-field*" i))
    (setf sf (combine sf (read-number 
             (send (symbol-value (read-from-string str)) :text)))))
  (setf sf (rest sf))
  (dotimes (i *nrate*)
    (setf str (format nil "*b~d-field*" i))
       (setf nf (combine nf (read-number 
             (send (symbol-value (read-from-string str)) :text)))))
  (setf nf (rest nf))
  (send *sd-mod* :sf sf)
  (send *sd-mod* :nf nf)))
      
(defun analyze-rate ()
  (read-fields)
  (send *sd-mod* :analyze-rate))

(defun default-rate-vals ()
  (cond ((< *nrate* 3) (error "Too few rating categories"))
        ((= *nrate* 3) (list (list 173 154 101)
                             (list 8 41 92)))
        ((= *nrate* 4) (list (list 173 154 101 66)
                             (list 8 41 92 104)))
        ((= *nrate* 5) (list (list 173 154 101 66 57)
                             (list 8 41 92 104 172)))    
        ((= *nrate* 6) (list (list 173 154 101 66 57 46)
                             (list 8 41 92 104 172 174)))
        ((= *nrate* 7) (list (list 173 154 101 66 57 46 40)
                             (list 8 41 92 104 172 174 180)))
        ((= *nrate* 8) (list (list 173 154 101 66 57 46 40 35)
                             (list 8 41 92 104 172 174 180 185)))
        ((= *nrate* 9) (list (list 173 154 101 66 57 46 40 35 30)
                             (list 8 41 92 104 172 174 180 185 190)))
        ((= *nrate* 10) (list (list 173 154 101 66 57 46 40 35 30 25)
                        (list 8 41 92 104 172 174 180 185 190 195)))
        ((= *nrate* 11) (list (list 173 154 101 66 57 46 40 35 30 25 20)
                        (list 8 41 92 104 172 174 180 185 190 195 200)))
        ((= *nrate* 12) (list (list 173 154 101 66 57 46 40 35 30 25 20 15)
                        (list 8 41 92 104 172 174 180 185 190 195 200 205)))
                    
        (t (message-dialog 
"That's a lot of categories! 
Collapse the number of 
categories to 10 or fewer.")
           (error "Unrealistically large number of rating categories!"))))

(defun setup-rate ()
  (let ((enter-label (send text-item-proto :new "Enter Frequencies:"))
      (blank-label (send text-item-proto :new "        "))
      (s2-label (send text-item-proto :new "sig"))
      (s1-label (send text-item-proto :new "blk"))  
      (clear-button (send button-item-proto :new "Close Rate"
                     :action #'remove-graphs))
      (analyze-button (send button-item-proto :new "Analyze"
                     :action #'analyze-rate))
      (sep1 (send text-item-proto :new
                "________________________"))
      (afields nil)
      (bfields nil)
      (dx nil))
      (def *nrate* (first (get-value-dialog 
         "How many rating categories?" :initial 7)))
      (setf dx (default-rate-vals))
      (remove-graphs)
      (dotimes (i *nrate*)
        (setf str (format nil "*a~d-field*" i))
        (set (read-from-string str) 
           (send edit-text-item-proto :new (num-to-string (select (first dx) i))
                 :text-length 4))
        (setf afields (combine afields (read-from-string str))))    
      (dotimes (i *nrate*)
        (setf str (format nil "*b~d-field*" i))
        (set (read-from-string str) 
           (send edit-text-item-proto :new (num-to-string (select (second dx) i))
                 :text-length 4))
        (setf bfields (combine bfields (read-from-string str))))    
      (def *ratedio* (send dialog-proto :new (list
             enter-label        
             (combine s2-label (mapcar #'eval (rest afields)))
             (combine s1-label (mapcar #'eval (rest bfields)))
             sep1
             (list analyze-button clear-button))))
 	   (send *ratedio* :title "Rating Experiment")
     (setup-sn-all)
 	   (set-locs)
 	   (send *ratedio* :show-window)))

;; Conversion Dialog

(defun print-az (da-value)
  (let ((da (read-from-string (send da-value :text)))
        (az (da-to-az da)))
   (format t "~%d(a)= ~,4f    Az= ~,4f~%" da az)))
 
(defun print-da (az-value)
  (let ((az (read-from-string (send az-value :text))))
  (format t "~%Az= ~,4f    d(a)= ~,4f~%" az (az-to-da az))))

(defun sd-stats (d1-value s2-value c1-value)
  (send *sd-mod* :d1 (read-from-string (send d1-value :text)))
  (send *sd-mod* :s2 (read-from-string (send s2-value :text)))
  (send *sd-mod* :c1 (read-from-string (send c1-value :text)))
  (send *sd-mod* :print-roc-stats))
  
(defun setup-convert-dialog ()
 (let* ((foo1 (remove-graphs))
  (value-label-da (send text-item-proto :new "d(a):"))
  (value-label-az (send text-item-proto :new "A(z):"))
  (sep1 (send text-item-proto :new
                "_________________________________"))
  (sep2 (send text-item-proto :new "      "))
  (sep3 (send text-item-proto :new
                "_________________________________"))
  (da-value (send edit-text-item-proto :new "1.0" :text-length 8))
  (az-value (send edit-text-item-proto :new "0.7603" :text-length 8))
  (d1-value (send edit-text-item-proto :new "1.0" :text-length 4))
  (s2-value (send edit-text-item-proto :new "1.4" :text-length 4))
  (c1-value (send edit-text-item-proto :new "0.0" :text-length 4))
  (da-to-az-button (send button-item-proto :new "-->"
       :action #'(lambda () 
         (let* ((da (read-from-string (send da-value :text)))
                (az (da-to-az da)))
              (send az-value :text (num-to-string az))))))       
  (az-to-da-button (send button-item-proto :new "<--"
       :action #'(lambda ()
         (let* ((az (read-from-string (send az-value :text)))
                (da (az-to-da az)))
              (send da-value :text (num-to-string da))))))
  (value-label-d1 (send text-item-proto :new "d1:"))
  (value-label-s2 (send text-item-proto :new "s2:"))
  (value-label-c1 (send text-item-proto :new "c1:"))
  (clear-button (send button-item-proto :new "Close Convert" 
      :action #'remove-graphs))
;;   (help-button (send button-item-proto :new "Help" :action #'roc-help))
  (stats-button (send button-item-proto :new "Stats"
      :action #'(lambda () (sd-stats d1-value s2-value c1-value))))
  (condio nil))
  (unless (boundp '*sd-mod*) 
    (def *sd-mod* (send signal-proto :new '(1 2) '(3 4) :title "Subject 1?")))
  (setf condio (send dialog-proto :new (list
      (list (list value-label-da da-value da-to-az-button)
      (list value-label-az az-value az-to-da-button))
      sep1
      (list value-label-d1 d1-value) 
      (list value-label-s2 s2-value)
      (list value-label-c1 c1-value stats-button)
      sep3
;;       (list help-button clear-button))))
      (list clear-button))))
  (send condio :title "Convert Values")))
  
(defun install-normal-detect ()
  (let ((divide-item (send dash-item-proto :new))
      (dispose-item (send menu-item-proto :new "Remove Menu" 
          :action #'remove-sn))
      (normal-item (send menu-item-proto :new "Explore Normal Model" 
          :action #'setup-sn-dialog))
      (two-item (send menu-item-proto :new "Analyze 2X2" 
          :action #'setup-two))
      (rate-item (send menu-item-proto :new "Rating Analysis"
          :action #'setup-rate))
      (convert-item (send menu-item-proto :new "Convert Values"
          :action #'setup-convert-dialog)))
      (def *detect-menu* (send menu-proto :new "Signal Detection"))
      #-unix(send *detect-menu* :append-items two-item rate-item 
        normal-item convert-item divide-item dispose-item)
      #+unix(send *detect-menu* :append-items two-item rate-item 
        normal-item convert-item)
      (send *detect-menu* :install) ))

(install-normal-detect)

 