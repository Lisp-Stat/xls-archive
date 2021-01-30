(defvar *dist* (list "GAUSSIAN" "T" "CHISQ" "F" "BINOMIAL" "POISSON"))

(defun gaussian-quant (quant mu sigma)
 (+ mu (* (normal-quant quant) sigma)))

(defun gaussian-cdf (x mu sigma)
 (normal-cdf (/ (- x mu) sigma)))

(defvar GAUSSIAN- (list "Mu" "Sigma"))
(defvar T- (list "Df"))
(defvar CHISQ- (list "Df"))
(defvar F- (list "Num Df" "Denom Df"))
(defvar BINOMIAL- (list "n" "p"))
(defvar POISSON- (list "Mean"))

(defun param-dialog (p-text type fun)
  (let* (
         (p-ask (mapcar #'(lambda (x) (send text-item-proto :new 
                           (format nil "Enter ~a:" x))) p-text))
         (p-get (mapcar #'(lambda (x)
                   (send edit-text-item-proto :new "" :text-length 5)) p-text))
         (quant-cdf-ask (if type (send text-item-proto :new "Enter P-value: ")
                             (send text-item-proto :new "Enter Quantile: ")))
         (quant-cdf-get (send edit-text-item-proto :new "" :text-length 5))
         (ok (send modal-button-proto :new "Ok"
               :action #'(lambda ()
                   (let* ((text (map-elements #'send 
                                    (cons quant-cdf-get p-get) :text))
                          (values (mapcar #'(lambda (x)
                                    (read (make-string-input-stream x))) text))
                          (quant-cdf-val (apply #'funcall fun values)))
                      (list (rest values) quant-cdf-val (first values))))))
         (dialog (send modal-dialog-proto :new (append
                    (transpose (list p-ask p-get))
                    (list (list quant-cdf-ask quant-cdf-get) ok)))))
    (send dialog :modal-dialog)))

(defun result-dialog (p-names p-vals cdf-quant-val user-val choice type)
   (let* (
          (dist-tell (send text-item-proto :new 
                         (format nil "~a Distribution" choice)))
          (dash (send text-item-proto :new "--------------------------"))
          (p-tell (mapcar #'(lambda (x y) (send text-item-proto :new
                               (format nil "~a:   ~a" x y))) p-names p-vals))
          (user-val (if type 
                   (send text-item-proto :new
                        (format nil "Probability:   ~5,5f" user-val))
                   (send text-item-proto :new
                        (format nil "Quantile: ~7,4f" user-val))))
          (cdf-quant-tell (if type 
                   (send text-item-proto :new 
                        (format nil "Quantile: ~7,4f" cdf-quant-val))
                   (send text-item-proto :new 
                        (format nil "Probability:   ~5,5f" cdf-quant-val))))
         )
     (send dialog-proto :new (append (list dist-tell dash) (transpose 
                              (list (append p-tell 
                                      (list user-val cdf-quant-tell)))))
                        :title (format nil "~a Distribution" choice))))

(defun tables ()
 (let* (
        (item-list (send list-item-proto :new *dist*))
        (get-quant (send button-item-proto :new "Find Quantile"
                    :action #'(lambda ()
                     (let* ((choice (elt *dist* (send item-list :selection)))
                            (fun (intern (concatenate 'string choice "-QUANT")))
                            (param-names (symbol-value (intern 
                              (concatenate 'string choice "-"))))
                            (param-vals (param-dialog param-names t fun)))
                       (result-dialog param-names (first param-vals)
                                                  (second param-vals)
                                                  (third param-vals) 
                                                  choice t)))))
        (get-cdf (send button-item-proto :new "Evaluate CDF"
                    :action #'(lambda ()
                     (let* ((choice (elt *dist* (send item-list :selection)))
                            (fun (intern (concatenate 'string choice "-CDF")))
                            (param-names (symbol-value (intern 
                              (concatenate 'string choice "-"))))
                            (param-vals (param-dialog param-names nil fun)))
                       (result-dialog param-names (first param-vals)
                                                  (second param-vals) 
                                                  (third param-vals) 
                                                  choice nil)))))
       )
    (send dialog-proto :new (list item-list (list get-quant get-cdf))
                            :title "Electronic Table Dialog")))

(tables)



