;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This function laggs all of the variables in DATA by LAG except
;;  the variable LAG-VARIABLE where the first LAG values are removed.
;;  Function calls are of the form:
;;  (def mydata (lag-data DATA LAG &optional LAG-VARIABLE))
;;  where LAG is the number of observations to lag.  DATA is either a
;;  list of lists or a matrix, and LAG-VARIABLE is an index (0 based of
;;  course) of the variable that is to be used as the lag-variable.  Missing
;;  values are defined here as any non-numeric character and all rows of DATA
;;  above the lowest appearing missing value are removed.  The user is
;;  alerted each time of the number of rows removed.  Returned is the
;;  lagged differences along with the lag-variable, truncated to the correct
;;  number of places.  The differences and lagged variable appear in the
;;  same order as they were entered into the function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lag-data (data lag &optional (lag-variable 0))
 (let* (
        (matp (if (matrixp data) t nil))
        (data (cond ((listp data) (get-lag-nonmissing data))
                    ((matrixp data) (mapcar #'(lambda (x) (coerce x 'list))
                                   (column-list (get-lag-nonmissing data))))))
        (p (length data))
        (lag-var (elt data lag-variable))
        (n (length lag-var))
        (lagged-lag-var (select lag-var (iseq (- n lag))))
        (newdata (apply #'append (if (not (= lag-variable 0))
                                  (mapcar #'(lambda (x)
                                                (select x (iseq lag (1- n))))
                                    (select data (iseq 0 (1- lag-variable)))))
                              (list lagged-lag-var)
                              (if (not (= lag-variable (1- p)))
                                  (mapcar #'(lambda (x)
                                           (list (select x (iseq lag (1- n)))))
                                     (select data (iseq (1+ lag-variable)
                                              (1- p)))))))
       )
 (if matp (apply #'bind-columns newdata) newdata)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This function takes a lagged difference of the variables in DATA
;;  except for the lagged-variable which has the first LAG values removed.
;;  Function calls are of the form:
;;  (def my-diff-lag (difference-lag DATA LAG &optional LAG-VARIABLE))
;;  where DATA is either a matrix or a list of lists.  LAG is the number of
;;  values to lag the differences, and LAG-VARIABLE is an index (0 based
;;  of coarse) of the variable that is to be used as the lag variable.
;;  Missing values are defined here as the any non-numeric character
;;  and all rows of DATA above the lowest appearing missing value are
;;  removed.  The user is alerted each time of the number of rows lagged.
;;  Returned is the lagged differences along with the lag-variable,
;;  truncated to the correct number of places.  The differences and
;;  lagged variable appear in the same order as they were entered
;;  into the function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun difference-lag (data lag &optional (lag-variable 0))
 (let* (
        (matp (if (matrixp data) t nil))
        (data (cond ((listp data) (get-lag-nonmissing data))
                    ((matrixp data) (mapcar #'(lambda (x) (coerce x 'list))
                                   (column-list (get-lag-nonmissing data))))))
        (p (length data))
        (lag-var (elt data lag-variable))
        (n (length lag-var))
        (lagged-lag-var (select lag-var (iseq lag (1- n))))
        (newdata (apply #'append (if (not (= lag-variable 0))
                                  (mapcar #'(lambda (x)
                                     (- (select x (iseq (- n lag)))
                                        (select x (iseq lag (1- n)))))
                                    (select data (iseq 0 (1- lag-variable)))))
                              (list lagged-lag-var)
                              (if (not (= lag-variable (1- p)))
                                  (mapcar #'(lambda (x)
                                     (list (- (select x (iseq (- n lag)))
                                              (select x (iseq lag (1- n))))))
                                     (select data (iseq (1+ lag-variable)
                                                   (1- p)))))))
       )
 (if matp (apply #'bind-columns newdata) newdata)))







(defun get-lag-nonmissing (data &optional (miscodes nil))
"Args: DATA (&optional MISCODES)
DATA is either a list, a list of lists, or a matrix.  Each sublist (or column)
corresponds to a variable.  If MISCODES is not provided, all cases with
strictly numeric characters are returned.  If MISCODES is provided, it should 
be either a string or a list of strings of missing codes.  Missing codes are 
considered case-insensitive.  All cases without MISCODES in any position are 
returned."
(flet ((find-flag (row miscodes n)
         (let ((i 0))
           (loop
            (if (find t (mapcar #'(lambda (x) 
                          (string= x (format nil "~a" (elt row i)))) miscodes))
                (return nil)
                (if (< i n) (setf i (1+ i)) (return t)))))))
 (let* (
        (miscodes (if miscodes (mapcar #'string-upcase 
                         (if (listp miscodes) miscodes (list miscodes)))))
        (t-data (cond ((matrixp data) (row-list data))
                      ((and (car data) (listp (first data))) (transpose data))
                      (t (transpose (list data)))))
        (n (1- (length t-data)))
        (p (1- (length (first t-data))))
        (good-rows (if miscodes 
                    (mapcar #'(lambda (x) (find-flag x miscodes p)) t-data)
                    (mapcar #'(lambda (x)
                         (if (member nil (mapcar #'numberp (coerce x 'list)))
                             nil t)) t-data)))
        (start-good-rows (position nil (combine (mapcar #'(lambda (x) 
                         (if (member nil (select good-rows (iseq x n))) t nil)) 
                         (iseq 0 n)))))

       )

 (unless start-good-rows (error "Missing value for last observation!!"))
 (print (format t "There were ~a observations removed" start-good-rows))
 (if good-rows
    (cond ((matrixp data) (apply #'bind-rows (select t-data 
                                              (iseq start-good-rows n))))
          ((and (elt data 0) (listp (elt data 0))) 
             (transpose (select t-data (iseq start-good-rows n))))
          (t (combine (select t-data (iseq start-good-rows n)))))
    (error "There are no cases free of missing values"))
 )
))



