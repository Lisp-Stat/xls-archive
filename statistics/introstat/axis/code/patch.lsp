;;;;
;;;;       Replace/revise some of the default regression options.
;;;;



;;   Move arg processing to  :isnew rather than in the building
;;    function so that this processing is inherited by descendants.

(defun regression-model (&rest args)
  (apply #'send regression-model-proto :new args))

(defmeth regression-model-proto :isnew (x y &key
                                          (intercept T) 
                                          (print T) 
                                          weights
                                          (included (repeat t (length y)))
                                          predictor-names
                                          response-name
                                          case-labels)
  (format t "Building regression model for ~a... ~%" response-name)
  (let ((x (cond 
             ((matrixp x) x)
             ((vectorp x) (list x))
             ((and (consp x) (numberp (car x))) (list x))
             (t x)))  )
    (send self :needs-computing t)
    (send self :x (if (matrixp x) x (apply #'bind-columns x)))
    (send self :y y)
    (send self :intercept intercept)
    (send self :weights weights)
    (send self :included included)
    (send self :predictor-names predictor-names)
    (send self :response-name response-name)
    (send self :case-labels case-labels)
    (if print (send self :display))
    self))


;;  Fix a bug in the option processing of the :response-name method.

(defmeth regression-model-proto :response-name (&optional (name "Y" set))
"Message args: (&optional name)
With no argument returns the response name. NAME sets the name."
  (if set 
      (setf (slot-value 'response-name) 
            (if name (string name) "Y")))
  (slot-value 'response-name))


;;  Add routine to produce a printed ANOVA table.

(defmeth regression-model-proto :PRINT-ANOVA ()
  (let* ((resSS  (send self :sum-of-squares))
         (regrSS (- (send self :total-sum-of-squares) resSS))
         (resDF  (send self :df))
         (regrDF (1- (- (send self :num-cases) resDF)))
         ( F     (/ (/ regrSS regrDF) (/ resSS resDF)))   )
    (format t "ANOVA Table:~%")
    (format t
            "Source            SS        df       MS        F     p-value~%")
    (format t "Regression  ~12,3f  ~4d  ~10,3g  ~7,2f ~6,3f~%"
            regrSS regrDF (/ regrSS regrDF) F (- 1 (f-cdf F regrDF resDF)))
    (format t "Residual    ~12,3g  ~4d  ~10,3g~%"
            resSS  resDF  (/ resSS resDF))
    ))
   

