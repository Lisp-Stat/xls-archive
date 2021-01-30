;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;     Stepwise regression model prototype.  Inherits from         ;;;
;;;     regression-model-proto.  Forward, backward, and Maximum     ;;;
;;;     R-Squared selection is available through keywords.          ;;;
;;;     Selection can also be manualy controlled while viewing      ;;;
;;;     added variable plots using either of the forward,           ;;;
;;;     backward, or stepwise procedures by giving the keyword      ;;;
;;;     :procedure the argument :added-variable.                    ;;;
;;;                                                                 ;;;
;;;     Questions, comments to Jason Bond                           ;;;
;;;                            jbond@laplace.stat.ucla.edu          ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto stepwise-model-proto 
          '(not-mod-vars tolerances criteria procedure summary
            enter-alpha exit-alpha sm-dim enter-var exit-var) ()
            regression-model-proto)

(defmeth stepwise-model-proto :isnew ()
    (send self :needs-computing nil))
  
(defmeth stepwise-model-proto :compute ()
    (let* (
           (included (if-else (send self :included) 1 0))
           (basis (slot-value 'basis))
           (x (send self :x))
           (y (send self :y))
           (intercept (send self :intercept))
           (weights (send self :weights))
           (w (if weights (* included weights) included))
           (sm (make-sweep-matrix x y w))
           (p (- (array-dimension sm 0) 1))
           (tss (aref sm p p))
           (sweep-result
            (if intercept
              (sweep-operator sm (if basis (1+ basis) nil))
              (sweep-operator sm (cons 0 (if basis (1+ basis) nil)))))
          )
      (setf (slot-value 'sweep-matrix) (first sweep-result))
      (setf (slot-value 'total-sum-of-squares) tss)
      (setf (slot-value 'residual-sum-of-squares)
            (aref (first sweep-result) p p))
    )
)

(defmeth stepwise-model-proto :added-variable (newvar)
 (let (
       (sm (slot-value 'sweep-matrix))
       (old (find newvar (slot-value 'basis)))
       (predictor-names (send self :predictor-names))
      )
 (when old
     (setf (slot-value 'basis)
             (remove newvar (slot-value 'basis)))
     (setf (slot-value 'sweep-matrix)
             (first (sweep-operator (slot-value 'sweep-matrix) (list newvar)))))
 (let* (
        (x (column-list (send self :x)))
        (nseq (iseq (length x)))
        (residuals (send self :residuals))
        (partial-reg (regression-model (select x (remove newvar nseq))
                                       (elt x newvar) :print nil))
        (added-residuals (send partial-reg :residuals))
        (part-plot (plot-points residuals added-residuals :variable-labels
                     (list (elt predictor-names newvar)
                           "Partial Residuals")))
        (ask-variable (send self :ask-enter-remove newvar old))
       )
     (when old
      (setf (slot-value 'sweep-matrix) sm)
      (setf (slot-value 'basis)
            (sort-data (cons newvar (slot-value 'basis)))))
     (send part-plot :close)
     ask-variable
  )
 )
)

(defmeth stepwise-model-proto :ask-enter-remove (newvar old)
  (let* (
         (ask-var (send text-item-proto :new (format nil "~a ~a?"
                    (if old "Remove" "Add") 
                    (elt (send self :predictor-names) newvar))))
         (yep (send modal-button-proto :new "Yep"
                :action #'(lambda () t)))
         (nope (send modal-button-proto :new "Nope"
                :action #'(lambda () nil)))
         (dialog (send modal-dialog-proto :new (list ask-var (list yep nope))))
        )
    (send dialog :modal-dialog)
  )
)

(defmeth stepwise-model-proto :show-enter (enter-var print)
 (format t "~%______________________________")
 (format t "~%Variable Entered: ~a"
            (elt (send self :current-model-predictor-names)
                 (position enter-var (slot-value 'basis))))
 (format t "~%______________________________~%"))


(defmeth stepwise-model-proto :show-exit (exit-var print)
 (format t "~%______________________________")
 (format t "~%Variable Removed: ~a"
       (elt (send self :current-model-predictor-names)
            (position exit-var (slot-value 'basis))))
 (format t "~%______________________________~%")
)

(defmeth stepwise-model-proto :print-max-r ()
 (format t "~%____________________________________________________________")
 (format t "~%The above model is the best ~a variable model found."
            (length (slot-value 'basis)))
 (format t "~%____________________________________________________________~%")
)


 
(defmeth stepwise-model-proto :forward-step (print)
 (let ((enter-var (first (send self (send self :criteria) t))))
   (cond (enter-var
          (send self :sweep-variable enter-var)
          (send self :add-variable enter-var)
          (send self :summary t enter-var)
          (when print (send self :show-enter enter-var print)
                      (send self :display))
          t)
         (t (when print (format t "~%______________________________")
                        (format t "~%No Variables Entered")
                        (format t "~%______________________________~%"))
            nil))))

(defmeth stepwise-model-proto :backward-step (print)
  (let ((exit-var (send self (send self :criteria) nil)))
    (cond ((first exit-var)
           (send self :sweep-variable (first exit-var))
           (send self :summary nil (first exit-var))
           (when print (send self :show-exit (first exit-var) print))
           (send self :delete-variable (first exit-var))
           (when (second exit-var)
                (send self :sweep-variable (second exit-var))
                (send self :summary t (second exit-var))
                (send self :add-variable (second exit-var))
                (if print (send self :show-enter (second exit-var) print)))
           
           (if print (send self :display))
           t)
          (t (when print (format t "~%______________________________")
                         (format t "~%No Other Variables Removed")
                         (format t "~%______________________________~%"))
             nil))))

(defmeth stepwise-model-proto :start-stepwise (print bottom)
 (format t "~%~%~%Stepwise Selection Procedure for Dependant Variable ~a"
      (send self :response-name))
 (send self :compute)
 (if print (send self :display))
  (loop
   (cond (bottom 
             (setf forward-check (send self :forward-step print))
             (setf backward-check (send self :backward-step print)))
         (t (setf backward-check (send self :backward-step print))
            (setf forward-check (send self :forward-step print))))
   (if backward-check
            (loop (setf backward-remove (send self :backward-step print))
                  (if (not backward-remove) (return))))
   (if (not (or forward-check backward-check)) (return))
  )
 (send self :print-summary)
 (send self :display)
)

(defmeth stepwise-model-proto :start-forward (print)
 (format t "~%~%~%Forward Selection Procedure for Dependant Variable ~a"
      (send self :response-name))
 (send self :compute)
 (if print (send self :display))
  (loop
    (setf forward-check (send self :forward-step print))
   (if (not forward-check) (return))
  )
 (send self :print-summary)
 (send self :display)
)

(defmeth stepwise-model-proto :start-backward (print)
 (format t "~%~%~%Backward Elimination Procedure for Dependant Variable ~a"
      (send self :response-name))
  (send self :compute)
  (if print (send self :display))
  (loop (setf backward-check (send self :backward-step print))
     (if (not backward-check) (return)))
  (send self :print-summary)
  (send self :display)
)

(defmeth stepwise-model-proto :sweep-variable (var)
 (let* ((sm (sweep-operator (send self :sweep-matrix) 
                            (1+ (list var)) (list 0)))
        (sm-dim (send self :sm-dim)))
   (setf (slot-value 'residual-sum-of-squares)
           (aref (first sm) sm-dim sm-dim))
   (setf (slot-value 'sweep-matrix) (first sm))))


(defmeth stepwise-model-proto :add-variable (enter-var)
    (setf (slot-value 'basis) (sort-data 
                (append (slot-value 'basis) (list enter-var))))
    (send self :not-mod-vars (remove enter-var (send self :not-mod-vars)))
)

(defmeth stepwise-model-proto :delete-variable (exit-var)
    (setf (slot-value 'basis) (remove exit-var (slot-value 'basis)))
    (send self :not-mod-vars (sort-data 
                (append (send self :not-mod-vars) (list exit-var))))
)

(defmeth stepwise-model-proto :start-max-r-squared (print)
 (format t "~%~%~%Maximum R-Squared Procedure for Dependant Variable ~a"
      (send self :response-name))
 (send self :compute)
 (if print (send self :display))
 (send self :forward-step print)
 (loop
  (send self :forward-step print)
  (if (= (length (slot-value 'basis)) (1- (send self :sm-dim))) (return))
  (loop
    (setf backward-check (send self :backward-step print))
    (if (not backward-check) (return)))
 (if print (send self :print-max-r)))
 (send self :print-summary)
 (send self :display)
)


(defmeth stepwise-model-proto :max-r-squared (enter)
 (let* (
        (sm-dim (send self :sm-dim))
        (basis (slot-value 'basis))
        (not-mod-vars (send self :not-mod-vars))
        (rss-list (list ))
        (total-ss (slot-value 'total-sum-of-squares))
       )
  (cond (enter 
         (dolist (i not-mod-vars)
           (let ((sm (first (sweep-operator (slot-value 'sweep-matrix)
                              (1+ (list i))))))
            (setf rss-list (append rss-list (list (aref sm sm-dim sm-dim))))))
         (list (elt not-mod-vars (position (min rss-list) rss-list))))
        (t
          (let ((rss-obs (aref (slot-value 'sweep-matrix) sm-dim sm-dim)))
          (dolist (i basis)
            (let ((sm (first (sweep-operator (slot-value 'sweep-matrix)
                         (1+ (list i))))))
             (dolist (j not-mod-vars)
              (let ((sm (first (sweep-operator sm (1+ (list j))))))
                (setf rss-list (append rss-list
                       (list (- rss-obs (aref sm sm-dim sm-dim)))))))))
          (let ((maxposn (position (max rss-list) rss-list)
                             nil))
           (if (> (max rss-list) 0)
            (list (elt basis (floor (/ maxposn (length not-mod-vars))))
                  (elt not-mod-vars (mod maxposn (length not-mod-vars))))
            (list nil nil))))))))

(defmeth stepwise-model-proto :partial-f (enter)
  (let* (
         (sm-dim (send self :sm-dim))
         (basis (slot-value 'basis))
         (not-mod-vars (send self :not-mod-vars))
         (df (if enter (1- (send self :df)) (send self :df)))
         (alpha-level (if enter (- 1 (send self :enter-alpha))
                                (- 1 (send self :exit-alpha))))
         (f-vals (list ))
         (f-critical (f-quant alpha-level 1 df))
        )
    (dolist (i (if enter not-mod-vars basis))
      (let* (
             (sm (if enter 
                    (first (sweep-operator (slot-value 'sweep-matrix)
                            (1+ (list i))))
                    (slot-value 'sweep-matrix)))
             (rss (aref sm sm-dim sm-dim))
             (x-y-cov (aref sm sm-dim (1+ i)))
             (s-e (sqrt (* rss (aref sm (1+ i) (1+ i)))))
            )
        (setf f-vals (append f-vals (list (^ (* (sqrt df)
                                                (/ x-y-cov s-e)) 2))))))
    (send self :select-procedure enter f-vals f-critical)))

(defmeth stepwise-model-proto :select-procedure (enter obs-list critical)
 (let (
       (basis (slot-value 'basis))
       (not-mod-vars (send self :not-mod-vars))
       (maxlist nil)
      )
  (loop 
      (setf max-list (if enter (max obs-list) (min obs-list)))
      (setf newvar (cond ((and enter (> max-list critical))
                        (elt not-mod-vars (position max-list obs-list)))
                      ((and (not enter) (< max-list critical))
                        (elt basis (position max-list obs-list)))
                      (t nil)))
      (if (or (not newvar)
           (if (and newvar (send self :procedure))
               (send self (send self :procedure) newvar) t))
               (return (list newvar)))
      (setf not-mod-vars (if enter (remove (elt not-mod-vars
                              (position max-list obs-list)) not-mod-vars)))
      (setf basis (if (not enter) (remove (elt basis
                              (position max-list obs-list)) basis)))
      (setf obs-list (remove max-list obs-list)))))


(defmeth stepwise-model-proto :print-summary ()
 (let (
       (summary (send self :summary))
       (predictor-names (send self :predictor-names))
      )
   (format t "~%~%______________________________________________________")
   (format t "~%Summary Info")
   (format t "~%Step:               Entered:               Removed:~%")
   (mapcar #'(lambda (x y) (format t "~20a~23a~20a~%" x
               (if (first y) 
                   (elt (send self :predictor-names) (second y)) "")
               (if (first y) ""
                   (elt (send self :predictor-names) (second y)))))

      (iseq 1 (length summary)) summary)
   (format t "~%______________________________________________________")
   (format t "~%Final Model:")
   (format t "~%______________________________________________________")
 )
)

(defun stepwise-model (x y &key 
                           (intercept T)
                           (print T)
                           weights
                           (included (repeat t (length y)))
                           predictor-names
                           response-name
                           case-labels
                           (enter-alpha .15) (exit-alpha .15) 
                           forward backward
                           stepwise (bottom t)
                           max-r-squared 
                           (procedure nil)
                           (file nil))
(if file (dribble file))
(let* (
       (x (cond
             ((matrixp x) x)
             ((vectorp x) (list x))
             ((and (consp x) (numberp (car x))) (list x))
             (t x)))
       (stepwise (if (or stepwise (and (not stepwise) (not max-r-squared)
                          (not forward) (not backward))) t nil))
       (num-indep (cond
             ((matrixp x) (array-dimension x 1))
             ((vectorp x) (length x))
             ((and (consp x) (numberp (car x))) 1)
             (t (length x))))
       (stepwise-model (send stepwise-model-proto :new))
      )
  (send stepwise-model :criteria (if max-r-squared :max-r-squared :partial-f))
  (send stepwise-model :sm-dim (1+ num-indep))
  (send stepwise-model :slot-value 'basis
        (cond ((or (and stepwise bottom) forward max-r-squared)
                 nil)
               (t (iseq num-indep))))
  (send stepwise-model :x (if (matrixp x) x (apply #'bind-columns x)))
  (send stepwise-model :y y)
  (send stepwise-model :intercept intercept)
  (send stepwise-model :weights weights)
  (send stepwise-model :included included)
  (send stepwise-model :predictor-names predictor-names)
  (send stepwise-model :response-name response-name)
  (send stepwise-model :case-labels case-labels)
  (send stepwise-model :enter-alpha enter-alpha)
  (send stepwise-model :exit-alpha exit-alpha)
  (send stepwise-model :criteria (if max-r-squared :max-r-squared :partial-f))
  (send stepwise-model :procedure procedure)
  (send stepwise-model :not-mod-vars
        (cond ((or (and stepwise (not bottom)) backward)
                 nil)
              (t (iseq num-indep))))
  (cond (max-r-squared (send stepwise-model :start-max-r-squared print))
        (stepwise (send stepwise-model :start-stepwise print bottom))
        (forward (send stepwise-model :start-forward print))
        (backward (send stepwise-model :start-backward print)))
  (if file (dribble))
 stepwise-model
)
)


(defun stepwise-model-dialog (x y &key
                           (intercept T)
                           weights
                           (included (repeat t (length y)))
                           predictor-names
                           response-name
                           case-labels
                           (enter-alpha .15) (exit-alpha .15))
 (let* (
        (title (send text-item-proto :new "Stepwise Regression Model"))
        (underline (send text-item-proto :new 
              "--------------------------------------------"))
        (ask-model-type (send text-item-proto :new "Model Selection type:"))
        (get-model-type (send choice-item-proto :new 
             (list "Forward Stepwise" "Backward Stepwise" 
                   "Forward Selection" "Backward Elimination" "Max R-Squared")))
        (ask-procedure (send text-item-proto :new "Procedure:"))
        (get-procedure (send choice-item-proto :new 
             (list "Automatic" "Show Added-Variable Plots")))
        (ask-print (send text-item-proto :new "Print Info to:"))
        (file-window-tell (send text-item-proto :new "" :text-length 12))
        (file-window-show (send text-item-proto :new "" :text-length 30))
        (get-print (send choice-item-proto :new 
                     (list "Don't Print" "Screen" "File") :value 1
                      :action #'(lambda ()
                        (let (
                              (value (send self :value))
                              (file-name nil)
                             )
                         (cond ((= value 2)
                               (setf file-name (get-string-dialog
                                            "Enter Filename"))
                               (cond ((= 0 (length file-name))
                                      (send self :value 1)
                                      (send file-window-tell :text "")
                                      (send file-window-show :text ""))
                                     (t (send file-window-tell :text 
                                        (format nil "File Name:"))
                                        (send file-window-show :text
                                        (format nil "~a" file-name)))))
                               (t (send file-window-tell :text "")
                                  (send file-window-show :text "")))))))
        (alpha-change (send button-item-proto :new "View/Change alphas"
             :action #'(lambda () 
               (let* (
                      (a-enter (send text-item-proto :new "Alpha to Enter: "))
                      (a-exit (send text-item-proto :new "Alpha to Exit: "))
                      (show-a-enter (send edit-text-item-proto :new 
                            (format nil "~a" enter-alpha)))
                      (show-a-exit (send edit-text-item-proto :new
                            (format nil "~a" exit-alpha)))
                      (do-it (send modal-button-proto :new "Ok"
                         :action #'(lambda ()
                          (let ((new-a-enter (read (make-string-input-stream
                                   (send show-a-enter :text)) nil))
                                (new-a-exit (read (make-string-input-stream
                                   (send show-a-exit :text)) nil)))
                            (setf enter-alpha new-a-enter)
                            (setf exit-alpha new-a-exit)))))
                      (dialog (send modal-dialog-proto :new 
                                (list (list a-enter show-a-enter)
                                      (list a-exit show-a-exit) do-it)))
                     )
                  (send dialog :modal-dialog)))))
        (ok (send button-item-proto :new "Go"
             :action #'(lambda () 
                         (let* (
                                (model-type (send get-model-type :value))
                                (procedure (send get-procedure :value))
                                (print-val (send get-print :value))
                                (print (if (/= print-val 0) t nil))
                                (stepwise nil)
                                (bottom nil)
                                (forward nil)
                                (backward nil)
                                (max-r-squared nil)
                                (bottom t)
                                (step nil)
                                (file (send file-window-show :text))
                               )
                          (case model-type (0 (setf stepwise t))
                                           (1 (setf stepwise t)
                                              (setf bottom nil))
                                           (2 (setf forward t))
                                           (3 (setf backward t))
                                           (4 (setf max-r-squared t)))
                          (case procedure (0 (setf procedure nil))
                                          (1 (setf procedure :added-variable)))
                          (setf step
                             (stepwise-model x y :intercept intercept
                                  :print print
                                  :weights weights
                                  :included included
                                  :predictor-names predictor-names
                                  :response-name response-name
                                  :case-labels case-labels
                                  :enter-alpha enter-alpha
                                  :exit-alpha exit-alpha 
                                  :stepwise stepwise
                                  :forward forward
                                  :backward backward
                                  :bottom bottom
                                  :max-r-squared max-r-squared
                                  :procedure procedure
                                  :file file))))))
        (dialog (send dialog-proto :new
                      (list (list title) (list underline)
                        (list ask-model-type get-model-type)
                        (list ask-procedure get-procedure)
                        (list ask-print get-print) 
                        (list file-window-tell file-window-show)
                        (list alpha-change ok))))
       )
  )
)

(defmeth stepwise-model-proto :not-mod-vars (&optional (val nil set))
  (if set (setf (slot-value 'not-mod-vars) val)
  (slot-value 'not-mod-vars))
)

(defmeth stepwise-model-proto :enter-alpha (&optional (val nil set))
  (if set (setf (slot-value 'enter-alpha) val)
  (slot-value 'enter-alpha))
)

(defmeth stepwise-model-proto :exit-alpha (&optional (val nil set))
  (if set (setf (slot-value 'exit-alpha) val)
  (slot-value 'exit-alpha))
)

(defmeth stepwise-model-proto :sm-dim (&optional (val nil set))
  (if set (setf (slot-value 'sm-dim) val)
  (slot-value 'sm-dim))
)

(defmeth stepwise-model-proto :criteria (&optional (val nil set))
  (if set (setf (slot-value 'criteria) val)
  (slot-value 'criteria))
)

(defmeth stepwise-model-proto :procedure (&optional (val nil set))
  (if set (setf (slot-value 'procedure) val)
  (slot-value 'procedure))
)

(defmeth stepwise-model-proto :summary (&optional (enter nil set) var) 
  (if set (setf (slot-value 'summary) 
                (append (slot-value 'summary) 
                        (if enter (list (list t var)) (list (list nil var)))))
          (slot-value 'summary))
)

(defmeth stepwise-model-proto :tolerances (&optional (val nil set))
  (if set (setf (slot-value 'tolerances) val)
  (slot-value 'tolerances))
)

(defmeth stepwise-model-proto :current-model-predictor-names 
           (&optional (names nil set))
"Message args: (&optional (names nil set))
With no argument returns the predictor names. NAMES sets the names."
  (if set (setf (slot-value 'predictor-names) (mapcar #'string names)))
  (let ((p (array-dimension (send self :x) 1))
        (p-names (slot-value 'predictor-names)))
    (if (not (and p-names (= (length p-names) p)))
        (setf (slot-value 'predictor-names)
              (mapcar #'(lambda (a) (format nil "Variable ~a" a))
                      (iseq 0 (- p 1))))))
  (select (slot-value 'predictor-names) (slot-value 'basis)))



(defmeth stepwise-model-proto :display ()
  (let ((coefs (coerce (send self :coef-estimates) 'list))
        (se-s (send self :coef-standard-errors))
        (x (send self :x))
        (p-names (send self :current-model-predictor-names)))
    (if (send self :weights)
        (format t "~%Weighted Least Squares Estimates:~2%")
        (format t "~%Least Squares Estimates:~2%"))
    (when (send self :intercept)
          (format t "Constant~25t~13,6g~40t(~,6g)~%" (car coefs) (car se-s))
          (setf coefs (cdr coefs))
          (setf se-s (cdr se-s)))
    (dotimes (i (length (send self :basis)))
             (cond
               ((member (elt (send self :basis) i) (send self :basis))
                (format t "~a~25t~13,6g~40t(~,6g)~%"
                        (select p-names i) (car coefs) (car se-s))
                (setf coefs (cdr coefs) se-s (cdr se-s)))
               (t (format t "~a~25taliased~%" (select p-names i)))))
    (format t "~%")
    (format t "R Squared:~25t~13,6g~%" (send self :r-squared))
    (format t "Sigma hat:~25t~13,6g~%" (send self :sigma-hat))
    (format t "Number of cases:~25t~9d~%" (send self :num-cases))
    (if (/= (send self :num-cases) (send self :num-included))
        (format t "Number of cases used:~25t~9d~%" (send self :num-included)))
    (format t "Degrees of freedom:~25t~9d~%" (send self :df))
    (format t "~%")))


(defmeth stepwise-model-proto :coef-estimates ()
"Message args: ()
Returns the OLS (ordinary least squares) estimates of the regression
coefficients. Entries beyond the intercept correspond to entries in basis."
  (let ((n (array-dimension (send self :x) 1))
        (indices (if (send self :intercept) 
                     (cons 0 (if (send self :basis) (+ 1 (send self :basis))))
                     (if (send self :basis) (+ 1 (send self :basis)))))
        (m (send self :sweep-matrix)))
    (coerce (compound-data-seq (select m (+ 1 n) indices)) 'list)))

(defmeth stepwise-model-proto :xtxinv ()
"Message args: ()
Returns ((X^T) X)^(-1) or ((X^T) W X)^(-1)."
  (let ((indices (if (send self :intercept)
                     (cons 0 (if (send self :basis) (1+ (send self :basis))))
                     (if (send self :basis) (1+ (send self :basis))))))
    (select (send self :sweep-matrix) indices indices)))



