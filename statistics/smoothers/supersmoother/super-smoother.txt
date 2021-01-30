(defproto supersmoother-proto '(x y w n r1 r2 r3 tsmo msmo wsmo jcv alpha span 
                           smoothed-y resid))

(defmeth supersmoother-proto :isnew (x y w span alpha)
"Args: X Y W SPAN ALPHA
X is the independant variable.  Y is the sequence to be smoothed with weights
W.  Span should be either a fixed span width to be used for all X's or NIL
for a Variable Span smooth.  If SPAN is non-NIL then ALPHA will be ignored
otherwise alpha is the Bass tone control parameter in the Variable Span 
smoother."
   (let* (
         (ord (order x))
         (n (length x))
         (newx (select x ord))
         (newy (select y ord))
         (neww (if w (select w ord) (repeat 1 n)))
        )
    (send self :x newx)
    (send self :y newy)
    (send self :w neww)
    (send self :alpha alpha)
    (send self :n n)
    (send self :span (if (numberp span) (floor span) span))
    (send self :start)
   )
)


(defmeth supersmoother-proto :smooth (var2 j res)
  (let* (
         (x (send self :x))
         (var2 (if res (send self var2) (abs (send self var2))))
         (w (send self :w))
         (n (send self :n))
         (bottom (floor (/ j 2)))
         (top (- n bottom 1))
         (xseq (select x (iseq 0 j)))
         (var2seq (select var2 (iseq 0 j)))
         (wseq (select w (iseq 0 j)))
         (wsum (sum wseq))
         (xbar (/ (sum (* xseq wseq)) wsum))
         (var2bar (/ (sum (* var2seq wseq)) wsum))
         (cov (sum (* wseq (- xseq xbar) (- var2seq var2bar))))
         (xvar (sum (* wseq (^ (- xseq xbar) 2))))
         (i 0)
         (smooth-vals (list ))
         (residuals (list ))
         (beta 0)
         (alpha 0)
         (smooth 0)
        )
     (loop
       (setf beta (/ cov xvar))
       (setf alpha (- var2bar (* beta xbar)))
       (setf smooth (+ alpha (* beta (elt x i))))
       (if res (progn
                 (setf residuals (cons (/ (- (elt var2 i) smooth) (- 1 (/ wsum)
                                     (/ (^ (- (elt x i) xbar) 2) xvar))) 
                         residuals))))
       (setf smooth-vals (cons smooth smooth-vals))
       (setf i (1+ i))
       (if (= i n) (return (if res (list (reverse smooth-vals) 
                                         (reverse residuals))
                                   (reverse smooth-vals))))
       (when (and (< i top) (> i bottom))
             (let* (
                    (wtop (elt w (+ i bottom)))
                    (wbot (elt w (- i bottom 1)))
                    (xt (elt x (+ i bottom)))
                    (xb (elt x (- i bottom 1)))
                    (vt (elt var2 (+ i bottom)))
                    (vb (elt var2 (- i bottom 1)))
                    (xtop (* xt wtop))
                    (xbot (* xb wbot)) 
                    (v2top (* vt wtop))
                    (v2bot (* vb wbot)) 
                    (wsum (sum wseq))
                    (wspluswt (+ wsum wtop))
                   )
               (setf xbar (/ (+ (* wsum xbar) xtop (- xbot))
                             (+ wspluswt (- wbot))))

               (setf var2bar (/ (+ (* wsum var2bar) v2top (- v2bot))
                             (+ wspluswt (- wbot))))

               (setf xbef (- xt (/ (+ (* xbar (- wspluswt wbot)) xbot)
                                    wspluswt)))

               (setf vbef (- vt (/ (+ (* var2bar (- wspluswt wbot)) v2bot)
                                   wspluswt)))

               (setf cov (+ cov (* (/ wspluswt wsum) wtop xbef vbef)

                            (- (* (/ (- wspluswt wbot) wspluswt)
                                   wbot (- xb xbar) (- vb var2bar)))))
                                    
               (setf xvar (+ xvar (* (/ wspluswt wsum) (^ xbef 2) wtop)
                             (- (* (/ (- wspluswt wbot) wspluswt)
                                   wbot (^ (- xb xbar) 2)))))
             )
            (setf xseq (select x (iseq (- i bottom) (+ i bottom))))
            (setf var2seq (select var2 (iseq (- i bottom) (+ i bottom))))
            (setf wseq (select w (iseq (- i bottom) (+ i bottom))))
        )
     )
 )
)



(defmeth supersmoother-proto :start ()
(let (
      (span (send self :span))
     )
 (cond ((not span)
  (let* ( 
         (n (send self :n))
         (alpha (if (send self :alpha) (send self :alpha) nil))
         (tweet (max 2 (floor (* .05 n))))
         (mid (max 2 (floor (* .2 n))))
         (woof (max 2 (floor (* .5 n))))
         (tweetsmo (send self :smooth :y tweet t))
         (midsmo (send self :smooth :y mid t))
         (woofsmo (send self :smooth :y woof t))
         (temp (progn (send self :tsmo (first tweetsmo))
                      (send self :msmo (first midsmo))
                      (send self :wsmo (first woofsmo))
                      (send self :r1 (second tweetsmo))
                      (send self :r2 (second midsmo))
                      (send self :r3 (second woofsmo))))
         (tresidsmo (send self :smooth :r1 mid nil))
         (mresidsmo (send self :smooth :r2 mid nil))
         (wresidsmo (send self :smooth :r3 mid nil))
         (temp (progn (send self :r1 tresidsmo)
                      (send self :r2 mresidsmo)
                      (send self :r3 wresidsmo)))
         (bspanest (map-elements #'(lambda (x y z) 
                          (cond ((= x (min x y z)) tweet)
                               ((= y (min x y z)) mid)
                               (t woof))) tresidsmo mresidsmo wresidsmo))
         (minresid (mapcar #'min tresidsmo mresidsmo wresidsmo))
         (jcv (if alpha (send self :jcv (map-elements #'(lambda (x y) 
                                    (+ x (* (- woof x) (^ y (- 10 alpha)))))
                                  bspanest (/ minresid wresidsmo)))
                         (send self :jcv bspanest)))
       
         (jcvsmo (send self :smooth :jcv mid nil))
        )
    (send self :resid minresid)
    (send self :jcv (mapcar #'(lambda (x) (cond ((> x woof) woof)
                                                ((< x tweet) tweet)
                                                (t x))) jcvsmo))
    (send self :interpolate-smooth tweet mid woof)
  )
 )
 (t  (let* (
            (n (send self :n))
            (mid (max 2 (floor (* .2 n))))
            (smoothed-y (send self :smooth :y span t))
            (temp (send self :r1 (second smoothed-y)))
            (smoothed-resid (send self :smooth :r1 mid nil))
           )
       (send self :smoothed-y (first smoothed-y))
       (send self :resid smoothed-resid)
      ))
 )
)
)



(defmeth supersmoother-proto :interpolate-smooth (tweet mid woof)
  (let* (
         (n (send self :n))
         (tsmooth (send self :tsmo))
         (msmooth (send self :msmo))
         (wsmooth (send self :wsmo))
         (jcvsmo (send self :jcv))
         (f-list (- jcvsmo mid))
         (smoothed-y (list ))
        )
     (dotimes (i n)
       (setf smoothed-y (cons (if (> (elt f-list i) 0) 
             (interpolate (elt f-list i) mid woof (elt msmooth i) 
                          (elt wsmooth i))
             (interpolate (- (elt f-list i)) tweet mid 
                          (elt msmooth i) (elt tsmooth i))) smoothed-y)))
   (send self :smoothed-y (reverse smoothed-y))
  )
)


(defun interpolate (f span1 span2 smooth1 smooth2)
  (let (
        (g (/ f (- span2 span1)))
       )
    (+ (* (- 1 g) smooth1) (* g smooth2))
  )
)




(defmeth supersmoother-proto :x (&optional (val nil set))
   (if set (setf (slot-value 'x) val)
   (slot-value 'x))
)

(defmeth supersmoother-proto :y (&optional (val nil set))
   (if set (setf (slot-value 'y) val)
   (slot-value 'y))
)

(defmeth supersmoother-proto :w (&optional (val nil set))
   (if set (setf (slot-value 'w) val)
   (slot-value 'w))
)

(defmeth supersmoother-proto :r1 (&optional (val nil set))
   (if set (setf (slot-value 'r1) val)
   (slot-value 'r1))
)

(defmeth supersmoother-proto :r2 (&optional (val nil set))
   (if set (setf (slot-value 'r2) val)
   (slot-value 'r2))
)

(defmeth supersmoother-proto :r3 (&optional (val nil set))
   (if set (setf (slot-value 'r3) val)
   (slot-value 'r3))
)

(defmeth supersmoother-proto :tsmo (&optional (val nil set))
   (if set (setf (slot-value 'tsmo) val)
   (slot-value 'tsmo))
)

(defmeth supersmoother-proto :msmo (&optional (val nil set))
   (if set (setf (slot-value 'msmo) val)
   (slot-value 'msmo))
)

(defmeth supersmoother-proto :wsmo (&optional (val nil set))
   (if set (setf (slot-value 'wsmo) val)
   (slot-value 'wsmo))
)

(defmeth supersmoother-proto :jcv (&optional (val nil set))
   (if set (setf (slot-value 'jcv) val)
   (slot-value 'jcv))
)


(defmeth supersmoother-proto :alpha (&optional (val nil set))
   (if set (setf (slot-value 'alpha) val)
   (slot-value 'alpha))
)

(defmeth supersmoother-proto :span (&optional (val nil set))
   (if set (setf (slot-value 'span) val)
   (slot-value 'span))
)

(defmeth supersmoother-proto :smoothed-y (&optional (val nil set))
   (if set (setf (slot-value 'smoothed-y) val)
   (slot-value 'smoothed-y))
)

(defmeth supersmoother-proto :n (&optional (val nil set))
   (if set (setf (slot-value 'n) val)
   (slot-value 'n))
)

(defmeth supersmoother-proto :resid (&optional (val nil set))
   (if set (setf (slot-value 'resid) val)
   (slot-value 'resid))
)





(defun smooth-dialog ()
   (let* (
          (var1 (variables))
          (vars (mapcar #'string var1))
          (xask (send text-item-proto :new "X:"))
          (yask (send text-item-proto :new "Y:"))
          (wask (send text-item-proto :new "W:"))
          (xget (send choice-item-proto :new vars))
          (yget (send choice-item-proto :new vars))
          (wget (send choice-item-proto :new vars))
          (add-item (send toggle-item-proto :new "Add to current plot?"))
          (line1 (send text-item-proto :new
                      "__________________________________________"))
          (t1 (send text-item-proto :new "Choose Smoothing Parameters"))
          (ask-span (send text-item-proto :new "Span Value:"))
          (span-value (send text-item-proto :new "Span to be Used:"))
          (span-window (send text-item-proto :new "" :text-length 10))
          (span-choice (send list-item-proto :new 
            (list "Tweeter" "Midrange" "Woofer" "Automatic" "User Defined")
            :selection 2))
          (alpha-window (send text-item-proto :new "" :text-length 40))
                                                 
          (clear-button (send button-item-proto :new "Clear Lines"
                         :action #'(lambda () 
                          (send smooth-plot :clear-lines)))) 
          (ok (send button-item-proto :new "Smooth it Baby!!"
                      :action #'(lambda ()
                      (let* (
                             (v1 (symbol-value (elt var1 (send xget :value)))) 
                             (v2 (symbol-value (elt var1 (send yget :value))))
                             (v3 (symbol-value (elt var1 (send wget :value))))
                             (lv1 (length v1))
                             (lv2 (length v2))
                             (lv3 (length v3))
                             (v4 (send span-choice :slot-value 'span-val))
                             (v5 (send span-choice :slot-value 'alpha))
                             (v6 (send add-item :value))
                            )
                         (if (numberp v4) (if (not (< 1 v4 (1+ lv1))) 
                                    (error "Bad Span!!")))
                         (if (not (= lv1 lv2 lv3)) 
                             (error "Sequences not of same length!!!"))
                         (setf v4 (cond ((numberp v4) (floor v4))
                                        ((string= v4 "t") (floor (* lv1 .05)))
                                        ((string= v4 "m") (floor (* lv1 .2)))
                                        ((string= v4 "w") (floor (* lv1 .5)))
                                        ((string= v4 "a") nil)))
                         (if (numberp v4) (setf v4 (max 2 v4)))
                         (setf smooth 
                           (send supersmoother-proto :new v1 v2 v3 v4 v5))
                           (if (not v6) 
                           (setf smooth-plot (plot-points (send smooth :x) 
                                                 (send smooth :y))))
                           (send smooth-plot :add-lines (send smooth :x) 
                                             (send smooth :smoothed-y))


                      )
                     )
              )
          )
         )
        (send span-choice :add-slot 'span-val)
        (send span-choice :add-slot 'alpha)
        (send span-choice 
            :action #'(lambda (x)
                    (let (
                          (val (send span-choice :selection))
                         )
                    (if (= val 4) (progn
                     (let ( 
                           (spval (get-string-dialog "Enter Span Value:"))
                          )
                     (if spval (setf spval (read 
                                        (make-string-input-stream spval) nil)))

                     (if (not (numberp spval)) 
                              (progn
                                 (setf val 0)
                                 (send span-choice :selection 0)
                              ))
                     (send span-choice :slot-value 'span-val spval))))
                (send span-window :text 
                           (cond  ((= val 4) (format nil "~a"
                                  (send span-choice :slot-value 'span-val)))
                                  ((= val 0) 
                                   (send span-choice :slot-value 'span-val "t")
                                             "Tweeter")
                                  ((= val 1) 
                                   (send span-choice :slot-value 'span-val "m")
                                             "Midrange")
                                  ((= val 2) 
                                   (send span-choice :slot-value 'span-val "w")
                                             "Woofer")
                                  ((= val 3) 
                                   (send span-choice :slot-value 'span-val "a")
                                             "Automatic")))
                     (if (= val 3)
                          (progn
                             (let* (
                                    (get-alpha (choose-item-dialog 
                                               "Pick an alpha:"
                                               '("0" "1" "2" "3" "4" "5"
                                                 "6" "7" "8" "9" "10" 
                                                 "No Bass Enhancement")))
                                   )
                                (if (not get-alpha) (setf get-alpha 0))
                                (send alpha-window
                       :text (format nil "Bass Tone Control:  ~a" 
                             (if (= get-alpha 11) "No Bass Enhancement"
                                 get-alpha)))
                                (send span-choice :slot-value 'alpha get-alpha)
                              )
                          )
                         (progn (send alpha-window :text "")
                                (send span-choice :slot-value 'alpha nil))
                     ))))
                
         (send dialog-proto :new
                        (list t1 (list xask yask wask)
                                 (list xget yget wget) (list line1)
                                 (list ask-span span-choice) 
                                 (list span-value span-window)
                                 (list alpha-window) (list add-item)
                                 (list ok clear-button))
                             :title "Smoother Dialog")
         
    )
)


(require "pr.lsp")
(def eps (normal-rand 200))
(def x (uniform-rand 200))
(def y (+ (sin (* 2 pi (^ (- 1 x) 2))) (* x eps)))
(undef 'eps)
(def w1 (reverse (/ (iseq 200) (sum (iseq 200)))))
(def w2 (repeat 1 200))
(smooth-dialog)



