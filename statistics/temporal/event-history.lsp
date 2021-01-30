;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Data is a list which indicates at which of a number of discrete time
;; points The Event occurred for each individual (starting at time 0).
;; Anything larger than or equal to cut is considered to be right 
;; censored. There are time-varying covariates, in a matrix of
;; individuals times time-points, with each element a list. The
;; function expand-data prepares input for the glim module.
;; It has a keyword parameter which indicates how the hazard rate
;; varies as function of time.
;;
;; This needs to be expanded to multiple events, to multiple kinds of
;; events, to Cox regression, and to continuous time.
;; 
;; Version 0.1 -- 07-14-95 -- Jan de Leeuw
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expand-data (data cut covariates &key (time-function #'dummy-time))
  (let (pred
        crit)
    (dolist (i data (values pred crit))
            (let ((time (1+ i)))
              (dotimes (k time)
                       (let ((int (funcall time-function k cut))
                             (pre (aref covariates i k)))
                       (setf pred (append pred (list (append int pre))))
                       (setf crit (append crit (list
                                                (if (< k i) 0
                                                    (if (>= k cut) 0 1)))))
                         )
                       )
              )
            )
    )
  )

(defun dummy-time (time top)
  (let ((e (repeat 0 top)))
    (if (= time top) nil
    (setf (elt e time) 1))
    e
    )
  )

(defun linear-time (time cut)
  (list 1 time)
  )

(defun log-linear-time (time cut)
  (list 1 (log (1+ time)))
  )

;; (setf data '(0 1 2 2 3 4 5 4 2 2))
;; (setf covariates (make-array '(10 5) :initial-element '(1 2)))
