(defmeth binary-models-proto :logit
  (y x &optional (maxiter 25) (maxgrad .0001) &key (starter nil))
  (flet ((new-raph 
          (x var resid)
          (let* ((x-prime (transpose x))
                 (inv-h (inverse 
                         (matmult x-prime 
                                  (apply #'bind-rows (* (row-list x) var)))))
                 (q (matmult x-prime resid)))
            (list (matmult inv-h q) inv-h q)))
         (predict
          (x beta)
          (let ((exp (exp (matmult x beta))))
            (/ exp (+ 1 exp )))))
    (do* ((iter 1 (+ iter 1))
          (var .25
               (* pred-val-p (- 1 pred-val-p)))
          (resid (- y .5) (- y pred-val-p))
          (ests (new-raph x var resid) (new-raph x var resid))
          (beta (select ests 0) (+ beta (select ests 0)))
          (pred-val-p (predict x beta) (predict x beta)))
         ((or (>= iter maxiter)(>= maxgrad (max (abs (select ests 2)))))
          (case starter
            (nil
             (let* ((var (* pred-val-p (- 1 pred-val-p)))
                    (xv (apply #'bind-rows (* (row-list x) (sqrt var))))
                      (trans-xv (transpose xv))
                    (hj (diagonal
                         (matmult xv 
                                  (inverse (matmult trans-xv xv))
                                  trans-xv)))
                    (resids-z (/ (- y pred-val-p) (sqrt var)))
                    (pred-val (matmult x beta))
                    (log1 (log pred-val-p))
                    (log2 (log (- 1 pred-val-p)))
                    (dj (- (* y (sqrt (* 2 (abs log1)))) 
                           (* (- 1 y) (sqrt (* 2 (abs log2))))))
                    (m1hj (- 1 hj))
                    (rsj (/ resids-z (sqrt m1hj)))
                    (delta-x-sq (^ rsj 2))
                    (var-cov (select ests 1))
                    (std-errs (sqrt (diagonal var-cov)))) 
               (send self :hj hj)
               (send self :delta-beta 
                       (/ (* delta-x-sq hj)
                          m1hj))
               (send self :delta-x-sq delta-x-sq)
               (send self :c-bar (* delta-x-sq hj))
               (send self :dfbeta 
                     (/ (mapcar
                         #'(lambda (xx)
                             (* xx
                                (/ (- y pred-val-p) m1hj)))
                         (column-list (matmult x var-cov)))
                        std-errs))
               (send self :delta-d 
                     (+ (^ dj 2)
                        (/ (* (^ resids-z 2) hj)
                           m1hj)))                 
               (send self :estimates beta)
               (send self :var-cov var-cov)
               (send self :grad (select ests 2))
               (send self :iterations iter)
               (send self :pred-val-p pred-val-p)
               (send self :pred-val pred-val)
               (send self :resids-z resids-z)
               (send self :rsj rsj)
               (send self :dj dj)
               (send self :z (+ pred-val resids-z))
               (send self :likelyhood (sum (+ (* y log1)
                                              (* (- 1 y) log2))))
               (send self :resids-y (- pred-val-p))
               (send self :std-errs std-errs)))
            (t beta))))))

(defmeth binary-models-proto :model-menu-maker ()
"Args: none
Sets up the model analysis menu for binary regression"
  (let ((parametric
         (send menu-item-proto :new "Parametric"
               :action
               #'(lambda ()
                   (send (select
                          (send 
                           *binary-model-object*
                           :visual-menu-items) 0) :enabled nil)
                           (def *binary-parametric-object*
                                (send binary-parametric-proto :new)))))
        (smooth
         (send menu-item-proto :new "Smooth"
               :action
               #'(lambda ()
                   (send (select
                          (send 
                           *binary-model-object*
                           :visual-menu-items) 1) :enabled nil)
                   (def *binary-smooth-object*
                        (send binary-smooth-proto :new))))))
    (send self :model-menu
          (send menu-proto :new "Model"))
    (send (send self :model-menu) :append-items
          smooth parametric)
    (send (send self :model-menu) :install)))

(defmeth binary-models-proto :model-menu (&optional (menu nil set))
"Args: (&optional (menu nil set))
Sets or returns model menu"
  (when set (setf (slot-value 'model-menu) menu))
  (slot-value 'model-menu))

(defmeth binary-models-proto :visualize-menu-maker ()
"Args: none
Sets up the model visualization menu for parametric binary regression"
  (let ((para-visual
         (send menu-item-proto :new "Parametric Model"
               :action #'(lambda ()
                           (def *binary-parametric-visual-object*
                                (send binary-parametric-visual-proto
                                      :new 
                                      (send *binary-model-object* :int))))))
        (visual-menu (send menu-proto :new "Visualize"))
        (smooth-visual
         (send menu-item-proto :new "Smooth Model"
               :action #'(lambda ()
                           (def *binary-smooth-visual-object*
                                (send binary-smooth-visual-proto
                                      :new))))))
    (send para-visual :enabled nil)
    (send smooth-visual :enabled nil)
    (send visual-menu :append-items para-visual smooth-visual)
    (send visual-menu :install)
    (send self :visual-menu visual-menu)
    (send self :visual-menu-items (list para-visual smooth-visual))))

(defmeth binary-models-proto :x-mat-menu-maker ()
  (let ((drop 
         (send menu-item-proto :new "Remove"
               :action #'(lambda () 
                           (when (send self :x-names)
                                 (send self :drop)))))
        (add
         (send menu-item-proto :new "Reinstate"
               :action #'(lambda ()
                           (when (send self :x-unnames)
                                 (send self :add)))))
        (transform
         (send menu-item-proto :new "Transformations"
               :action #'(lambda () (send self :transformer))))
        (x-mat-menu
         (send menu-proto :new "X matrix")))
    (send x-mat-menu :append-items add drop transform)
    (send x-mat-menu :install)
    (send self :x-mat-menu x-mat-menu)))

(defmeth binary-models-proto :disable-s ()
  (send (select (send self :visual-menu-items) 1) 
        :enabled nil)
  (when
   (member
    '*binary-smooth-visual-object*
    (variables) :test 'equalp)
   (let ((bso *binary-smooth-visual-object*))
     (send (send bso :resp-fun-prob-plot) :dispose)
     (send (send bso :biplot) :dispose)
     (send (send bso :resid-plot) :dispose)
     (send (send bso :univariate-plot) :dispose)
     (send (send bso :obs-names-win) :dispose)
     (send (send bso :stats-window) :dispose)
     (undef '*binary-smooth-visual-object*)))
  (undef '*binary-smooth-object*))

(defmeth binary-models-proto :disable-p ()
  (send (select (send self :visual-menu-items) 0) 
        :enabled nil)
  (when
   (member
    '*binary-parametric-visual-object*
    (variables) :test 'equalp)
   (let ((bpo *binary-parametric-visual-object*))
     (send (send bpo :resp-fun-plot) :dispose)
     (send (send bpo :infl-plot) :dispose)
     (send (send bpo :resid-plot) :dispose)
     (send (send bpo :biplot) :dispose)
     (send (send bpo :obs-names-win) :dispose)
     (send (send bpo :stats-window) :dispose)
     (undef '*binary-parametric-visual-object*)))
  (undef '*binary-parametric-object*))

(defmeth binary-models-proto :drop ()
  (let* ((var-names (send self :x-names))
         (names-items-list
          (mapcar #'(lambda (x) (send toggle-item-proto :new x)) var-names))
         (out-label (send text-item-proto :new "Variables to remove"))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                       (let ((selections 
                              (mapcar
                               #'(lambda (x) (send x :value))
                                names-items-list)))
                         (when (member 't selections :test 'equalp)
                               (send self :disable-p)
                               (when (< (min (which selections)) 
                                        (length (send self :x-selector)))
                                     (send self :disable-s))
                               (send self :dropper selections))))))
         (cancel (send modal-button-proto :new "Cancel"))
         (dialog 
          (send modal-dialog-proto :new
                (list
                 (list
                  (cons out-label names-items-list))
                 (list ok cancel)))))
    (send dialog :default-button ok)
    (send dialog :modal-dialog)))

(defmeth binary-models-proto :dropper (drop)
  (let* ((x-selector (send self :x-selector))
         (led-vs (send self :linked-vars))
         (length-xs (length x-selector))
         (length-vs (length led-vs))
         (length-drop (length drop))
         (drop-x-sel (select drop (iseq length-xs)))
         (drop-led-vs (when led-vs
                            (select drop
                                    (+ length-xs (iseq length-vs)))))
         (dxs (when drop-x-sel (which drop-x-sel)))
         (dlv (when drop-led-vs (which drop-led-vs)))
         (dxs-list (select x-selector dxs))
         (dlv-list (select led-vs dlv))
         (dlv-unlist (select led-vs
                             (remove-if
                              #'(lambda (x)
                                  (member x dlv)) 
                              (iseq length-vs)))))
    (send self :x-selector
          (remove-if
           #'(lambda (x) (member x dxs-list)) x-selector))
    (send self :x-out (append (send self :x-out)  dxs-list))
    (send self :linked-vars-out
          (append (send self :linked-vars-out) dlv-list))
    (when (member "intercept"
                  (select (send self :full-x-names)
                          (send self :x-out))
                  :test 'equalp)
          (send self :int 0))
    (send self :linked-vars dlv-unlist))
  (let* ((lv (send self :linked-vars))
         (x-out (send self :x-out))
         (lvo (send self :linked-vars-out))
         (lv-to-wo (remove-if-not
                    #'(lambda (x) (member (select x 0) x-out)) lv))
         (lv (remove-if
              #'(lambda (x) (member (select x 0) x-out)) lv))
         (lvo-to-wo (remove-if-not
                    #'(lambda (x) (member (select x 0) x-out)) lvo))
         (lvo (remove-if
              #'(lambda (x) (member (select x 0) x-out)) lvo)))
    (send self :linked-vars lv)
    (send self :linked-vars-out lvo)
    (send self :linked-vars-way-out
          (append (send self :linked-vars-way-out) lv-to-wo lvo-to-wo))
    (when (send self :x-selector)
          (send self :x-selector (sort-data (send self :x-selector))))
    (when (send self :x-out)
          (send self :x-out (sort-data (send self :x-out))))))

(defmeth binary-models-proto :add ()
  (let* ((var-names (send self :x-unnames))
         (names-items-list
          (mapcar #'(lambda (x) (send toggle-item-proto :new x)) var-names))
         (out-label (send text-item-proto :new "Variables to reinstate"))
         (ok (send modal-button-proto :new "OK"
                   :action #'(lambda ()
                               (let ((selections 
                                      (mapcar
                                       #'(lambda (x) (send x :value))
                                       names-items-list)))
                                 (when (member 't selections :test 'equalp)
                                       (send self :disable-p)
                                       (when (< (min (which selections)) 
                                                (length (send self :x-out)))
                                             (send self :disable-s))
                                       (send self :adder selections))))))
         (cancel (send modal-button-proto :new "Cancel"))
         (dialog 
          (send modal-dialog-proto :new
                (list
                 (list
                  (cons out-label names-items-list))
                 (list ok cancel)))))
    (send dialog :default-button ok)
    (send dialog :modal-dialog)))

(defmeth binary-models-proto :adder (add)
  (let* ((x-out (send self :x-out))
         (lv-out (send self :linked-vars-out))
         (lv-wout (send self :linked-vars-way-out))
         (len-x-out (length x-out))
         (len-lv-out (length lv-out))
         (len-lv-wout (length lv-wout))
         (axs (when x-out (which (select add (iseq len-x-out)))))
         (alvo (when lv-out 
                     (which 
                      (select add (+ (iseq len-lv-out) len-x-out)))))
         (alvwo (when lv-wout (which (select add (+ (iseq len-lv-wout)
                                len-x-out len-lv-out)))))
         (list-xs (select x-out axs))
         (list-lvo (select lv-out alvo))
         (list-lvwo (select lv-wout alvwo))
         (unasx (when x-out (which
                             (mapcar
                              #'not (select add 
                                            (iseq len-x-out))))))
         (unalvo (when lv-out (which (mapcar 
                                      #'not (select add (+ (iseq len-lv-out)
                                                           len-x-out))))))
         (unalvwo (when lv-wout 
                        (which (mapcar
                                #'not 
                                (select add (+ (iseq len-lv-wout)
                                               len-x-out len-lv-out))))))
         (out nil)
         (way-out nil))
    (when list-xs
          (send self :x-selector (append (send self :x-selector) list-xs))
          (send self :x-out (select x-out unasx)))
    (when list-lvo
          (send self :linked-vars (append (send self :linked-vars) list-lvo))
          (send self :linked-vars-out (select lv-out unalvo)))
    (when list-lvwo
          (mapcar
           #'(lambda (x) 
               (let ((orig-var (select x 0))
                     (linked-var (select x 1))
                     (x-selector (send self :x-selector)))
                 (cond
                   ((member orig-var x-selector)
                    (send self :linked-vars
                          (append (send self :linked-vars) (list x))))
                   (t
                    (send self :x-selector 
                          (append x-selector (list linked-var)))
                    (send self :x-out
                          (remove orig-var (send self :x-out)))))))
           list-lvwo)
          (send self :linked-vars-way-out (select lv-wout unalvwo)))
    (mapcar #'(lambda (x) (if (member (select x 0) (send self :x-selector))
                              (setf out (append out (list x)))
                              (setf way-out (append way-out (list x)))))
            (send self :linked-vars-way-out))
    (send self :linked-vars-way-out way-out)
    (send self :linked-vars-out (append (send self :linked-vars-out)
                                        out))
    (send self :linked-vars-out (remove-duplicates 
                                 (send self :linked-vars-out)
                                 :test 'equalp))
    (if (member "intercept" (send self :x-names) :test 'equalp)
        (send self :int 1)
        (send self :int 0))
    (when (send self :x-selector)
          (send self :x-selector (sort-data (send self :x-selector))))
    (when (send self :x-out)
          (send self :x-out (sort-data (send self :x-out))))
    (when (send self :linked-vars-way-out)
          (send self :linked-vars-way-out 
                (remove-if-not
                 #'(lambda (x)
                     (member (select x 0) (send self :x-out)))
                 (send self :linked-vars-way-out))))))

(defmeth binary-models-proto :add-variable (vector name)
  (let ((n-ivs (send self :n-ivs))
        (full-x (send self :full-x)))
    (when (and 
           (or (listp vector) (vectorp vector))
           (= (length vector) (length (select full-x 0)))
           (not (equalp name ""))
           (stringp name))
          (let ((vector (select vector (send self :order-y))))
            (send self :full-x (append full-x (list vector)))
            (send self :full-x-names
                  (append (send self :full-x-names) (list name)))
            (send self :x-selector
                  (append (send self :x-selector) (list n-ivs)))
            (send self :linked-vars
                  (append (send self :linked-vars) (list nil)))
            (send self :n-ivs (+ 1 n-ivs))))))

(defmeth binary-models-proto :transformer ()
  (let* ((var-names (send self :x-use-names))
         (vars-item (send choice-item-proto :new var-names))
         (trans-name (send edit-text-item-proto :new ""
                           :text-length 6))
         (trans-item (send edit-text-item-proto :new ""
                           :text-length 10))
         (orig-var (send text-item-proto :new "Original variable"))
         (spacer (send text-item-proto :new ""))
         (new-name (send text-item-proto :new "Transformed variable name"))
         (trans (send text-item-proto :new "Transformation"))
         (ok
          (send modal-button-proto :new "OK"
                :action #'(lambda ()
                            (let* ((var-ind (send vars-item :value))
                                   (new-name (send trans-name :text))
                                   (trans-eq (send trans-item :text)))
                              (when (not (or (equalp new-name "")
                                             (equalp trans-eq "")))
                                    (send self :add-trans
                                          var-ind new-name 
                                          (with-input-from-string
                                           (s trans-eq)
                                           (read s)))
                                    (send self :disable-p))))))
         (cancel (send modal-button-proto :new "Cancel"))
         (dialog 
          (send modal-dialog-proto :new
                (list
                 (list
                  (list orig-var vars-item)
                  (list 
                   (list spacer)
                   (list new-name trans-name)
                   (list trans trans-item)))
                  (list spacer ok cancel)))))
    (send dialog :default-button ok)
    (send dialog :modal-dialog)))

(defmeth binary-models-proto :add-trans (var-ind new-name trans)
  (let* ((int (send self :int))
         (x-selector (send self :x-selector))
         (index (select x-selector (+ int var-ind)))
         (full-x (send self :full-x))
         (orig-x (coerce (select full-x index) 'list))
         (trans-x (mapcar
                   #'(lambda (xxx) (eval (subst xxx 'x trans))) orig-x)))
    (send self :linked-vars 
          (append (send self :linked-vars)
                  (list (list index (length full-x) trans))))
    (send self :full-x-names
          (append (send self :full-x-names) (list new-name)))
    (send self :full-x (append (send self :full-x) (list trans-x)))))

