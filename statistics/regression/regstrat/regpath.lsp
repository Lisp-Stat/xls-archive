;;; Copyright 1993 Julian J Faraway
;;; Version 1.0

;; A LISP-STAT program to analyze the effect of applying the actions
;; in a regression analysis in different orders 
;; by Julian Faraway, Dept. of Statistics, Univ. of Michigan, faraway@umich.edu

(require "regmodel")

(defvar *reg-raps* '(
;		     :log-transform
		     :outlier-test
		     :test-influence
		     :restore-points
;		     :hetero-test
		     :box-cox-test
		     :tran-predictors
		     :bw-elim
		     :fw-sel))

(defvar *reg-raps-names* '(
;			   "Test variables for a log transform"
			   "Test for outliers"
			   "Test for influential points"
			   "Restore points"
;			   "Test for heteroscedascity"
			   "Box-Cox test"
			   "Transform predictors"
			   "Backward elimination"
			   "Forward Selection"))

(defvar *reg-raps-abb* '("o" "i" "r" "y" "t" "b" "f"))

(defun regpaths (x y &key 
		   predictor-names	
		   response-name 
		   tran-list)
  (let* ((iraps (car (choose-raps *reg-raps-names*)))
         (raps (select *reg-raps* iraps))
         (nraps (select *reg-raps-names* iraps))
         (no-vars (length x))
         (est-int (estimates-interest-dialog predictor-names no-vars))
         (vb-select (vb-selector est-int no-vars))
         (infolevel (verbosity-dialog))
         (rm (graph-analysis2 x y raps
			      :infolevel infolevel
			      :predictor-names predictor-names
			      :response-name response-name
			      :vb-select vb-select
			      :tran-list tran-list))	 
	 (ar (select *reg-raps-abb* iraps)))
    (format t "Code     Action~%")
    (dotimes (i (length ar))
	     (format t "~6a ~a~%" (nth i ar) (nth i nraps)))
    (mygraph rm ar)))

(defun choose-raps (names)
  (let ((ans (choose-subset-dialog "Regression actions" names)))
    (or ans (choose-raps names))))

(defun verbosity-dialog ()
  (let ((ans (choose-item-dialog "Verbosity of output" 
		      '("none" "some" "all"))))
    (or ans (verbosity-dialog))))

(defun estimates-interest-dialog (pn l)
  (car (choose-subset-dialog "Variables forced to be included" 
			     (or pn
            (mapcar #'(lambda (x) (format nil "~A" x)) 
                    (iseq 1 l))))))

(defun vb-selector (i n)
  (let ((tmp (repeat t n)))
    (if i
        (progn
         (setf (select tmp i) 
               (repeat '(nil) (length i)))
         tmp) tmp)))

(defun isterminal (cm)
  (let ((done t)
	(rml (mapcar #'car (send cm :children))))
    (dolist (i rml done)
	    (if (null (eql i cm))
		(return nil)))))

(defun copy-reg-object (r pnames)
  (let ((rm (send reg-model-proto :new)))
    (send rm :x (bind-row2 (copy-seq (row-list (send r :x)))))
    (send rm :y (copy-seq (send r :y)))
    (send rm :print nil)
    (send rm :intercept (send r :intercept))
    (send rm :weights (copy-seq (send r :weights)))
    (send rm :included (copy-seq (send r :included)))
    (send rm :predictor-names pnames)
    (send rm :response-name (send r :response-name))
    (send rm :case-labels (send r :case-labels))
    (send rm :tran-list (copy-seq (send r :tran-list)))
    (send rm :vb-select (send r :vb-select))
    (send rm :history (copy-seq (send r :history)))
    (send rm :origx (send r :origx))
    rm))

(defun dotr (rl a &rest arg)
  (if arg (mapcar #'(lambda (r) (send r a arg)) rl)
    (mapcar #'(lambda (r) (send r a)) rl)))


(defun graph-analysis2 (x y raps &key (infolevel 1) predictor-names
			response-name vb-select tran-list)
  (let* ((rmods (list (reg-model x y :print nil
				 :predictor-names predictor-names
				 :response-name response-name
				 :vb-select vb-select
				 :tran-list tran-list)))
	 (nraps (length raps))
	 (verbose (= infolevel 2))
	 (info (= infolevel 1))
	 (adjr (list (send (car rmods) :adjusted-rsquared))))
    (do ((ci 0 (+ ci 1)))
	((> ci (1- (length rmods))) (info-return rmods))
	(let* ((cm (nth ci rmods))
	       (ancestors (send cm :ancestors))
;; Avoid doing the same action twice by looking at the ancestors of a model
	       (tried (mapcar #'cadr ancestors))
	       (untried (if (car tried)
			    (set-difference (iseq 0 (1- nraps)) tried)
			  (iseq 0 (1- nraps)))))
	  (if verbose
	      (format t "Regmodel ~a unanalyzed - starting analysis~%" ci))
	  (dolist (i untried)
		  (let* ((copym (copy-reg-object cm predictor-names))
			 (action (nth i raps))
			 (changed (send copym (nth i raps))))
		    (if verbose
			(format t "Regmodel ~a changed by ~a ? ~a~%"
				ci (nth i raps) changed))
		    (if changed
			(let* ((ar (send copym :adjusted-rsquared))
			       (index (car (which (= ar adjr)))))
			  (if index
			      (progn
				(if verbose
				    (format t "Seen it before: model ~a ~%" index))
				(if info
				  (format t "Model ~a: Action: ~a Maps to Model ~a:~%"
					  ci (nth i raps) index))
				(send (nth index rmods) :ancestors
				      (list ci i))
				(send cm :children (list index i)))
			    (progn
			      (if verbose
				  (format t "Not seen it before - call it model ~a~%"
					  (length rmods)))
			      (if info
				  (format t "Model ~a: Action: ~a Maps to Model ~a:~%"
					  ci (nth i raps) (length rmods)))
			      (send copym :children nil)
			      (send copym :ancestors (list ci i))
			      (setf adjr (append adjr (list ar)))
			      (send cm :children (list (length rmods) i))
			      (setf rmods (append rmods (list copym)))))))))
	  (if (isterminal cm) (send cm :terminal-node t))))))

(defun info-return (rl)
  (cons (car rl)
	(mapcar #'(lambda (r)
		    (list (send r :children)
			  (send r :ancestors)
			  (send r :tran-list)
			  (send r :weights))) rl)))

(defun extract-ancestors (rl)
  (let ((cl (mapcar #'cadr  rl)))
    (mapcar #'(lambda (l) (mapcar #'car l)) cl)))

(defun extract-children (rl)
  (let ((cl (mapcar #'car rl)))
    (mapcar #'(lambda (l) (mapcar #'car l)) cl)))

(defun flatten (l)
    (if (= (length l) 1) (car l)
      (append (car l) (flatten (cdr l)))))

(defun extract-rpaths (rl raps)
   (let ((cl (mapcar #'car rl)))
    (mapcar #'(lambda (l) 
		(mapcar #'(lambda (z) (nth (cadr z) raps)) l)) cl)))

(defun extract-graph-info (cl)
  (let ((high (1- (length cl))))
    (do* ((result '(0) (cons cm result))
	  (beg 0 (1+ end))
          (end 0 cm)
          (cm (max (car cl)) (max (flatten (select cl (iseq beg end))))))
         ((= cm high) (cons 1 (difference (reverse (cons high result))))))))

(defun bind-row2 (vl)
  (let ((nc (length vl))
	(nr (length (car vl))))
    (make-array (list nc nr) :initial-contents vl)))

(defproto mygraph-proto
  '(graph-info node-radius node-centers ancestors children
	       active-node drag-center rpaths root-model
	       weights tran-lists display-model terminal-nodes
	       model-weights)
  '()
  graph-window-proto
  "Regression analysis graph")

(defun mygraph (rl raps &key (node-radius 10))
  (let ((g (send mygraph-proto :new))
	(cl (extract-children (cdr rl))))
    (send g :graph-info (extract-graph-info cl))
    (send g :node-radius node-radius)
    (send g :children cl)
    (send g :root-model (car rl))
    (send g :weights (mapcar #'fourth (cdr rl)))
    (send g :tran-lists (mapcar #'third (cdr rl)))
    (send g :ancestors (extract-ancestors (cdr rl)))
    (send g :rpaths (extract-rpaths (cdr rl) raps))
    (send g :title "Regression analysis graph")
    (send g :start-menu)
    g))

(defmeth mygraph-proto :graph-info (&optional (val nil set))
  (if set (setf (slot-value 'graph-info) val))
  (slot-value 'graph-info))

(defmeth mygraph-proto :root-model (&optional (val nil set))
  (if set (setf (slot-value 'root-model) val))
  (slot-value 'root-model))

(defmeth mygraph-proto :display-model (&optional (val nil set))
  (if set (setf (slot-value 'display-model) val))
  (slot-value 'display-model))

(defmeth mygraph-proto :weights (&optional (val nil set))
  (if set (setf (slot-value 'weights) val))
  (slot-value 'weights))

(defmeth mygraph-proto :tran-lists (&optional (val nil set))
  (if set (setf (slot-value 'tran-lists) val))
  (slot-value 'tran-lists))

(defmeth mygraph-proto :rpaths (&optional (val nil set))
  (if set (setf (slot-value 'rpaths) val))
  (slot-value 'rpaths))

(defmeth mygraph-proto :children (&optional (val nil set))
  (if set (setf (slot-value 'children) val))
  (slot-value 'children))

(defmeth mygraph-proto :ancestors (&optional (val nil set))
  (if set (setf (slot-value 'ancestors) val))
  (slot-value 'ancestors))

(defmeth mygraph-proto :model-weights (&optional (val nil set))
  (if set 
      (setf (slot-value 'model-weights) (/ val (sum val)))
  (slot-value 'model-weights)))

(defmeth mygraph-proto :terminal-nodes ()
  (if (null (slot-value 'terminal-nodes))
      (let ((tn (which (mapcar #'null (send self :children))))
	    (tl NIL))
	(send self :model-weights (repeat 1 (length tn)))
	(setf (slot-value 'terminal-nodes) (dolist (i tn tl)
		(push (send self :rm i :print nil) tl)))))
  (slot-value 'terminal-nodes))

      
(defmeth mygraph-proto :initialize-centers ()
  (let* ((gd (send self :graph-info))
         (cw (send self :canvas-width))
         (ch (send self :canvas-height))
         (nr (send self :node-radius))
         (nd (* 2 nr))
         (vspacing (round (/ (- ch (* 2 nd)) (1- (length gd)))))
         (yc nd)
         (centers nil))
    (dolist (i gd)
            (let ((hspacing (round (/ cw (1+ i)))))
              (dotimes (j i)
                       (let ((xc (* (1+ j) hspacing)))
                         (push (list xc yc) centers)))
              (setf yc (+ yc vspacing))))
    (send self :node-centers (reverse centers))))

(defmeth mygraph-proto :resize ()
  (let ((centers (send self :node-centers))
	(cw (send self :canvas-width))
	(ch (send self :canvas-height))
	(nr (send self :node-radius)))
    (if centers
	(let* ((mh (max (mapcar #'car centers)))
	       (mv (max (mapcar #'cadr centers)))
	       (fh (/ (- cw (* 2 nr)) mh))
	       (fv (/ (- ch (* 2 nr)) mv))
	       (nc (mapcar #'(lambda (z) (round (* (list fh fv) z))) centers)))
	  (send self :node-centers nc))
      (send self :initialize-centers))))

(defmeth mygraph-proto :redraw ()
  (let* ((nr (send self :node-radius))
         (nd (* 2 nr))
         (centers (send self :node-centers)))
    (send self :erase-window)
    (dolist (i centers)
	    (let ((xc (car i))
		  (yc (cadr i)))
	      (send self :frame-oval (- xc nr) (- yc nr) nd nd)))
    (send self :highlite-term )
    (send self :label-terminal-nodes)))

(defmeth mygraph-proto :highlite-term ()
  (let ((tn (which (mapcar #'null (send self :children))))
        (nd (* 2 (send self :node-radius)))
        (cn (send self :node-centers)))
    (if tn
        (progn
         (send self :line-width 3)
         (dolist (i tn)
                 (let* ((ce (nth i cn))
                        (ha (- (car ce) (/ nd 2)))
                        (va (- (cadr ce) (/ nd 2))))
                   (send self :frame-oval ha va nd nd)))
         (send self :line-width 1)))))

(defmeth mygraph-proto :node-radius (&optional (val nil set))
  (if set (setf (slot-value 'node-radius) val))
  (slot-value 'node-radius))

(defmeth mygraph-proto :node-centers (&optional (val nil set))
  (if set (setf (slot-value 'node-centers) val))
  (slot-value 'node-centers))

(send mygraph-proto :delete-method :do-motion)

(defmeth mygraph-proto :do-click (x y m1 m2)
  (let ((distances nil)
        (ncent (send self :node-centers))
        (nrad (send self :node-radius))
        (mc (list x y)))
    (flet ((dist (x1 x2) (sqrt (+ (^ (- (car x1) (car x2)) 2)
                                  (^ (- (cadr x1) (cadr x2)) 2)))))
	  (dolist (coords ncent)
		  (push (dist coords mc) distances))
	  (let ((sel (which (< (reverse distances) nrad))))
	    (if sel
		(send self :active-node (car sel)))
	    (if (and sel (send self :display-model))
		(setf *cm* (send self :rm (car sel))))
	    (if (and sel (send self :drag-center))
		(let* ((ac (nth (car sel) ncent))
		       (hc (car ac))
		       (vc (cadr ac))
		       (cu (send self :cursor)))
		  (send self :cursor 'hand)
		  (let ((xy (send self :drag-grey-rect 
				  hc vc 1 1)))
		    (send self :erase-oval (- hc nrad) (- vc nrad)
			  (* 2 nrad) (* 2 nrad))
		    (send self :frame-oval (- (car xy) nrad) (- (cadr xy) nrad)
			  (* 2 nrad) (* 2 nrad))
		    (setf (select ncent (car sel)) (select xy '(0 1)))
		    (send self :node-centers ncent)
		    (send self :cursor cu))))))))

(defmeth mygraph-proto :active-node (&optional (val nil set))
  (if set (setf (slot-value 'active-node) val))
  (slot-value 'active-node))

(defmeth mygraph-proto :drag-center (&optional (val nil set))
  (if set (setf (slot-value 'drag-center) val))
  (slot-value 'drag-center))

(defmeth mygraph-proto :draw-all-arcs ()
  (send self :redraw)
  (let ((cent (send self :node-centers))
        (cl (send self :children))
	(rp (send self :rpaths)))
    (dotimes (i (length cent))
             (let ((hc (car (nth i cent)))
                   (vc (cadr (nth i cent)))
                   (cc (nth i cl))
		   (rs (nth i rp)))
               (dotimes (j (length cc))
                       (let ((hd (car (nth (nth j cc) cent)))
                             (vd (cadr (nth (nth j cc) cent)))
			     (s (nth j rs)))
                         (send self :dirline hd vd hc vc s)))))))

(defmeth mygraph-proto :start-menu ()
  (let* ((mm (send menu-proto :new "Control"))
         (move (send menu-item-proto :new "Move node"
                     :action #'(lambda ()
                                 (send self :drag-center
                                       (not (send self :drag-center)))
				 (if (send self :display-model)
				     (progn
				       (send display :mark nil)
				       (send self :display-model nil)))
                                 (send move :mark (send self :drag-center)))))
         (display (send menu-item-proto :new "Display"
                        :action #'(lambda ()
                                 (send self :display-model
                                       (not (send self :display-model)))
				 (if (send self :drag-center)
				     (progn
				       (send move :mark nil)
				       (send self :drag-center nil)))
				 (send display :mark 
				       (send self :display-model)))))
         (clear (send menu-item-proto :new "Clear arcs"
                      :action #'(lambda () (send self :redraw))))
         (drawall (send menu-item-proto :new "Draw all"
                        :action #'(lambda () (send self :draw-all-arcs))))
         (weights (send menu-item-proto :new "Model Weights"
                      :action #'(lambda () (send self :set-weights))))
         (coeffs (send menu-item-proto :new "Effects"
                      :action #'(lambda () (send self :est-den))))
         (predict (send menu-item-proto :new "Prediction"
                        :action #'(lambda () (send self :pred-den)))))
    (send mm :append-items clear drawall move display weights predict coeffs)
    (let ((vb (send (send self :root-model) :vb-select)))
      (if (and (car vb) (allsame vb)) (send coeffs :enabled nil)))
    (send self :menu mm)))

(defmeth mygraph-proto :set-weights ()
  (let ((w (car (get-value-dialog "Model Weights"))))
    (if w (send self :model-weights w))))


(defun myatan (x y)
  (cond ((= x 0) (if (> y 0) (/ pi 2) (* pi 1.5)))
        ((< x 0) (+ pi (atan (/ y x))))
        ((> x 0) (if (>= y 0) (atan (/ y x))
                     (+ (* 2 pi) (atan (/ y x)))))))

(defmeth mygraph-proto :dirline (x1 y1 x2 y2 s)
  (let* ((xm (round (/ (+ x1 x2) 2)))
         (ym (round (/ (+ y1 y2) 2)))
         (phi (myatan (- x2 x1) (- y2 y1)))
         (xc (round (* 10 (cos phi))))
         (yc (round (* 10 (sin phi))))
         (the (/ pi 4))
	 (xs (round (+ xm (abs (* 7 (cos (+ phi (/ pi 2))))))))
	 (ys (round (+ ym (* 7 (sin (+ phi (/ pi 2)))))))
         (xa1 (round (+ xm (* 5 (cos (+ phi the))))))
         (ya1 (round (+ ym (* 5 (sin (+ phi the))))))
         (xa2 (round (+ xm (* 5 (cos (- phi the))))))
         (ya2 (round (+ ym (* 5 (sin (- phi the)))))))
    (send self :draw-line (+ x1 xc) (+ y1 yc) (- x2 xc) (- y2 yc))
    (send self :draw-line xm ym xa1 ya1)
    (send self :draw-line xm ym xa2 ya2)
    (send self :draw-string s xs ys)))

(defmeth mygraph-proto :rm (i &key (print T))
  (let* ((r (send self :root-model))
	 (x (send r :origx))
	 (y (send r :y))
	 (w (nth i (send self :weights)))
	 (tl (nth i (send self :tran-lists)))
	 (ty (funcall (caar tl) y))
         (vb (send r :vb-select))
	 (ny (send r :response-name))
	 (nx (send r :predictor-names)))
    (reg-model x ty :weights w :tran-list tl
	       :print print
               :vb-select vb
	       :response-name ny
	       :predictor-names nx)))

(defun tranden (f mu sig c)
    (let ((g (cadr (assoc f *deriv*))))
      (* (normal-dens (/ (- (funcall f c) mu) sig))
         (funcall g c) (/ 1 sig))))

(defmeth mygraph-proto :label-terminal-nodes ()
  (let ((nn (which (mapcar #'null (send self :children))))
	(mc (send self :node-centers))
	(nr (round (/ (send self :node-radius) 2))))
    (dotimes (j (length nn))
	    (let ((mn (format nil "~a" (1+ j)))
		  (nc (nth (nth j nn) mc)))
	      (send self :draw-string mn 
		    (- (car nc) nr) (+ (cadr nc) nr))))))

(defmeth mygraph-proto :pred-den ()
  (let* ((x (car (get-value-dialog "Enter point")))
	 (tn (send self :terminal-nodes))
         (pr (mapcar #'(lambda (r) (send r :predun x)) tn))
         (npr (length pr))
	 (w (send self :model-weights))
         (ts (format nil "Prediction at ~a" x))
         (pp (mapcar #'(lambda (x) (inverse-apply (car x) (cadr x))) pr))
         rmin rmax)
    (dotimes (j npr)
	    (let* ((i (nth j pr))
		   (ytf (car i))
		   (pred (nth 1 i))
		   (err (nth 2 i))
		   (ate (* (abs (- (inverse-apply  ytf (- pred (* 0.05 err)))
				   (inverse-apply  ytf (+ pred (* 0.05 err)))))
			   10)))
	      (push (inverse-apply  ytf (- pred (* 3 err))) rmin)
	      (push (inverse-apply  ytf (+ pred (* 3 err))) rmax)
	      (format t "Model no. ~a~%" (1+ j))
	      (print-model-summary1 (nth j tn) 
				    (inverse-apply ytf pred) ate (nth j w))))
    (let ((c (rseq (min rmin) (max rmax) 50))
          (d (repeat 0 50)))
      (dotimes (j npr)
	       (let ((i (nth j pr))
		     (wt (nth j w)))
		 (setf d (* (+ (tranden 
				(car i) (nth 1 i) (nth 2 i) c) d) wt))))
      (setf rmin (plot-lines c d :title ts))
      (send rmin :add-points pp (repeat 0 npr)))))

(defmeth mygraph-proto :est-den ()
  (let* ((tn (send self :terminal-nodes))
	 (w (send self :model-weights))
	 (ytfs (mapcar #'(lambda (r) (caar (send r :tran-list))) tn)))
    (if (allsame ytfs)
	(progn
	  (format t "All y-transforms are ~a~%" 
		  (caar (send (car tn) :tran-list)))
	  (plotsame tn w))
      (progn
	(format t "y-transforms are different~%")
	(plotdiff tn w)))))

(defun plotsame (tn w)
  (let* ((vb (send (car tn) :vb-select))
	 (pnames (select (send (car tn) :predictor-names)
                         (which (mapcar #'not vb))))
	 (ests nil) (errs nil))
    (dotimes (j (length w))
	    (let* ((i (nth j tn))
		   (sel (indic-vars-sel (cdr (send i :tran-list)) vb))
		   (coeffs (cdr (send i :coef-estimates)))
		   (cerrs (cdr (send i :coef-standard-errors))))
	      (format t "Model no. ~a~%" (1+ j))
	      (print-model-summary1 i (select coeffs sel) 
				    (select cerrs sel) (nth j w))
	      (push (select coeffs sel) ests)
	      (push (select cerrs sel) errs)))
    (dotimes (i (length pnames))
	     (plotest (mapcar #'(lambda (x) (nth i x)) ests)
                     (mapcar #'(lambda (x) (nth i x)) errs)
                     (nth i pnames) w))))

(defun print-model-summary1 (i ests errs w)
  (let ((rw (send i :weights)))
    (format t "Y-transform is ~a~%" (caar (send i :tran-list)))
    (format t "Variables: ~a~%" (vnames (send i :tran-list)
					(send i :predictor-names)))
    (format t "Model Weight is ~a~%" w)
    (if (and rw (allsame rw)) (format t "Point(s) ~a are excluded~%"
		   (1+ (which (= rw 0))))
      (format t "No points are excluded~%"))
    (format t "Adjusted R^2 is ~a~%" (send i :adjusted-rsquared))
    (format t "Estimates are ~a~%" ests)
    (format t "Stderrs are   ~a~%~%" errs)))
	
(defun plotdiff (tn w)
  (let* ((mp (interpret-point-dialog (send (car tn) :origx)))
         (ests (mapcar #'(lambda (r) (send r :decon-ests mp)) tn))
         (errs (mapcar #'(lambda (r) (send r :decon-stderrs mp)) tn))
         (pnames (select (send (car tn) :predictor-names)
                         (which (mapcar #'not (send (car tn) :vb-select))))))
    (dotimes (j (length w))
	     (let ((i (nth j tn))
		   (format t "Model no. ~a~%" (1+ j))
	    (print-model-summary1 i (nth j ests) (nth j errs) (nth j w)))))
    (dotimes (i (length pnames))
            (plotest (mapcar #'(lambda (x) (car (nth i x))) ests)
                     (mapcar #'(lambda (x) (car (nth i x))) errs)
                     (nth i pnames) w))))

(defun plotest (ests errs pn w)
  (let* ((rmin (min (- ests (* 3 errs))))
         (rmax (max (+ ests (* 3 errs))))
         (grid (rseq rmin rmax 50))
         (le (length ests))
         (p (repeat 0 50)))
    (dotimes (i le)
            (setf p (* (+ p (/ (normal-dens 
                          (/ (- grid (nth i ests)) (nth i errs)))
                            (nth i errs))) (nth i w))))
    (setf rmin (plot-lines grid p :title pn))
    (send rmin :add-points ests (repeat 0 le))))

(defun interpret-point-dialog (x)
  (let ((imp (choose-item-dialog 
              "Parameter Interpretation point?"
              '("median" "mean" "Average over predictors" "other"))))
    (case imp
      (0 (mapcar 'median x))
      (1 (mapcar 'mean x))
      (2 (transp x))
      (3 (car (get-value-dialog "Enter point")))
      (t (mapcar 'median x)))))


(defun allsame (l)
  (let ((f (car l))
	(r T))
    (dolist (i (cdr l) r)
	    (setf r (and r (equal f i))))))
