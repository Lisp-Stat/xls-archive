(defun bootstrap-demo (data)
"Args: data
Driver for one-dimensional bootstrap program. Puts up
dialog for choosing the function to be bootstrapped,the
number of samples, and the type of displays. 
The only argument is the sequence
DATA, from which the bootstrap samples are drawn."
(let* (
     (args (bootstrap-dialog))
     (f (intern (string-upcase (elt args 0))))
     (n (elt args 1))
     (p (elt args 2))
	    (s (elt args 3))
     (b (make-list n))
     (x (coerce data 'list))
     (q (histogram nil :title "Bootstrap Distribution" :show nil))
     (d (histogram data :title "The Data" :show nil))
     )
(send q :location 35 200)
(send q :size 500 150)
(send q :show-window)
(cond ((> p 0) (send d :location 300 20) (send d :show-window)))
(dotimes (i n)
    (setf (nth i b)
          (if (< i p) 
          (funcall f (boot-histogram x d s))
          (funcall f (sample x n t))))
    (if (= i p) (send d :close))
    (send q :add-points (list (nth i b)))
    (send q :draw-string (num-to-string (1+ i)) 20 20)
    (send q :adjust-to-data)
    )
(send q :add-lines (kernel-dens b))
(send q :add-lines (fit-normal b))
(send q :add-lines (list (repeat (quantile b .025) 2) (list 0 n)) :type 'dashed)
(send q :add-lines (list (repeat (quantile b .975) 2) (list 0 n)) :type 'dashed)
))

(defun bootstrap-dialog ()
"Args: none
Dialog to select user parameters" 
(let* (
  	 (first-label (send text-item-proto :new "Function to be Bootstrapped"))
  	 (second-label (send text-item-proto :new "Number of Bootstraps"))
  	 (third-label (send text-item-proto :new "Number of Full Displays"))
  	 (fourth-label (send text-item-proto :new "Pause in Full Displays"))
  	 (first-reply (send edit-text-item-proto :new "mean"))
  	 (second-reply (send edit-text-item-proto :new "100"))
  	 (third-reply (send edit-text-item-proto :new "3"))
  	 (fourth-reply (send edit-text-item-proto :new "5"))
  	 (cancel (send modal-button-proto :new "Cancel"))
  	 (ok (send modal-button-proto :new "OK" 
           :action #'(lambda()
                     (list
                     (send first-reply :text)
                     (dconvert (coerce (send second-reply :text) 'list))
                     (dconvert (coerce (send third-reply :text) 'list))
                     (dconvert (coerce (send fourth-reply :text) 'list))))
           ))
  (choice-dialog (send modal-dialog-proto :new
         (list 
         (list
         (list first-label second-label third-label fourth-label)
		       (list first-reply second-reply third-reply fourth-reply))
         (list ok cancel)
         )
         :default-button ok)))
  (send choice-dialog :modal-dialog)
))

(defun boot-histogram (data win spd)
(let* (
      (n (length data))
      (w (histogram nil :title "Bootstrap Sample" ))
      (jj 0)
      (jo nil)
      )
(dotimes (i n)
(setf jj (random n))
(setf jo (append jo (list (elt data jj))))
(send win :selection (list jj))
(send w :add-points (list (elt data jj)))
(send w :draw-string (num-to-string (1+ i)) 20 20)
(send w :selection (list i))
(send w :adjust-to-data)
(pause spd)
)
(send w :close)
jo
))

;;; miscellaneous utilities

(defun dconvert (cs)
"Args: list
Converts a LIST of characters to an integer. Only
characters -, +, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 
are allowed. Leading zeroes or spaces are no problem.
Neither are trailing spaces."
(let  (
      (sgn 1)
      )
(cond ((member #\+ cs) (setf cs (rest (member #\+ cs))))
      ((member #\- cs) (setf cs (rest (member #\- cs)))
                       (setf sgn -1))
      )
(let*  (
       (tc (- (mapcar #'char-code cs) 48))
       (v (select tc (which (>= tc 0))))
       (b (reverse (** 10 (iseq (length v)))))
       )
(* sgn (sum (* b v)))
)))

(defun fit-normal (x)
"Args: x
Computes mean and standard deviation, and returns list of
list of arguments and list of ordinates, suitable for
add-lines in one-dim plots"
(let* (
       (m (mean x))
       (s (standard-deviation x))
       (u (max x))
       (v (min x))
       (a (/ (- u v) 50))
       (b (/ (+ u v) 2))
       (z (+ b (* a (- (iseq 51) 25))))
       (p (/ (normal-dens (/ (- z m) s)) s))
       )
(list z p)))

