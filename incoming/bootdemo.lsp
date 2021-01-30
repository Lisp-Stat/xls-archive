;;;
;;; DEMO: Bootstrapping de modelo de regresion simple
;;;

;; Kjetil Halvorsen, La Paz, May 1998
;;   Copyright:  GPL

;;; drawing one or multiple lines in plot:
(def only-one-line nil)

;;; Number of bootstrap replicaction:
(def boot-repl 50)

; definition of data and model to simulate:
(def x (iseq -3 10))
(def n (length x))
(def w (rseq 1 6 n))
(def a 1)
(def b 1)
(def mu (+ a (* b x)))
(def error (* w (normal-rand n)))
(def y (+ mu error))

;Model estimated by least squares:
(def model (regression-model x y))
(def res (send model :raw-residuals))
(def fit (send model :fit-values))
(def model-coef (send model :coef-estimates))

(def plot (plot-points x y))
(send plot :use-color t)
(send plot :title "Bootstrap demo")

(defun residual-boot-demo (b)
(let ((coefs ())
      (coef nil)
      (m nil))
  ;(send plot :clear-lines :draw nil)
   (send plot :draw-color 'red)
  (dotimes (i b (nreverse coefs))
      (setq m (regression-model x 
                                (+ fit (sampleb res)) :print nil))
      (setq coef (send m :coef-estimates))
      (apply #'send plot :abline coef)
      (push coef coefs)
      (when only-one-line
            (send plot :clear-lines :draw nil)))))

(defun pair-boot-demo (b)
(let ((coefs ())
      (coef nil)
      (m nil)
      (pair-data (transpose (list x y)))
      (boot-data nil))
  ;(send plot :clear-lines :draw nil)
   (send plot :draw-color 'blue)
  (dotimes (i b (nreverse coefs))
      (setq boot-data (transpose (sampleb pair-data)))
      (setq m (apply #'regression-model
                      (first boot-data) (second boot-data)
                     '(:print nil)))
      (setq coef (send m :coef-estimates))
      (apply #'send plot :abline coef)
      (push coef coefs)
      (when only-one-line
            (send plot :clear-lines :draw nil)))))


;;;
;;; Menu support:
;;;

(setq bootdemo-menu (send menu-proto :new "BootDemo"))
(setq clearlines-item (send menu-item-proto :new "Clear Lines"
                            :action #'(lambda ()
                                        (send plot :clear-lines
                                              :draw nil))))
(setq b-item (send menu-item-proto :new "B"
                   :action #'(lambda ()
                               (setq boot-repl (car (get-value-dialog "B"
                                                    :initial boot-repl))))))
(setq ls-item (send menu-item-proto :new "Draw LS line:"
                       :action #'(lambda ()
                                   (send plot :draw-color 'green)
                                   (send plot :abline
                                          (first model-coef)
                                          (second model-coef)))))
                                        

(setq residualboot-item (send menu-item-proto :new "Residual Bootstrap"
                              :action #'(lambda ()
                                          (residual-boot-demo boot-repl))))
(setq pairboot-item (send menu-item-proto :new "Pair Bootstrap"
                          :action #'(lambda ()
                                      (pair-boot-demo boot-repl))))
(setq remove-item (send menu-item-proto :new "Remove"
                        :action #'(lambda ()
                                    (send bootdemo-menu :remove)
                                    (send bootdemo-menu :dispose)
                                    (send plot :remove)
                                    (undef 
                                     '(A B BOOT-REPL ERROR FIT MODEL MODEL-COEF MU N ONLY-ONE-LINE PLOT RES W X Y)))))

(setq explain-item (send menu-item-proto :new "Explain"
                         :action #'(lambda ()
      (princ "   A short bootstrap demo:       ") (terpri)
      (princ "Data is simulated from a straight line model, ") (terpri)
      (princ "where the variance increases with x. Observ ") (terpri)
      (princ "how this is reflected in the bootstrapped lines from") (terpri)
      (princ "the pairs bootstrap, but not from the residual bootstrap.") (terpri))))

(send bootdemo-menu :append-items clearlines-item
                                  b-item
                                  ls-item
                                  residualboot-item
                                  pairboot-item
                                  explain-item
                                  remove-item)

(send bootdemo-menu :install)

;;;
;;;  Because of an error in inbuilr sample function,
;;;  we write this one for use to make bootstrap resamples:
;;;

(defun sampleb (x)
(let ((n (length x)))
  (select x (random (repeat n n)))))


;;;;;;    EOF   ;;;;;;;;;;;;;;;