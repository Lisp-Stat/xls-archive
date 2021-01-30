;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This is a bit ad-hoc, but it may be useful for multilevel people.
;; We have a list of lists in predictors, and a list in criterium,
;; and a final list indicating group membership (coded by the
;; integers 0,...,ngroup.
;;
;; The function first computes group means, group sizes, and
;; the decomposition of the dispersion in within and between.
;;
;; It then runs separate regressions in each group,
;; a total regression on the pooled data, a (weighted)
;; between regression on group averages, a within regression
;; on deviations from the group mean, and an analysis of
;; covariance.
;;
;; In each case it first runs a null model (without predictors,
;; only with an intercept), and then the model for the selected
;; predictors.
;;
;; The example at the end has 10 selected schools from the
;; NELS-88 data set.
;;
;; Version 1.0 -- 07-18-95 -- Jan de Leeuw
;; Version 1.1 -- 07-18-95 -- Jan de Leeuw
;;                            Added within-group dispersions
;; Version 1.2 -- 08-04-95 -- Added null model to ANCOVA
;;                            Added contextual analysis
;;                            Added Cronbach model
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun between-within-total (predictors criterium indicator)
"Args: (predictors criterium indicator)
We have a list of lists in PREDICTORS, and a list in CRITERIUM,
and a final list INDICATOR indicating group membership (coded by the
integers 0,...,ngroup.

The function first computes group means, group sizes, within
group dispersions, and the decomposition of the dispersion in 
within and between.

It then runs separate regressions in each group,
a total regression on the pooled data, a (weighted)
between regression on group averages, a within regression
on deviations from the group mean, and an analysis of
covariance."

  (let* ((m (1+ (max indicator)))
         (mm (iseq m))
         (rr (make-string 60 :initial-element #\=))
         (g (mapcar 
             #'(lambda (z)
                 (if-else (= indicator z) 1 0)) 
             mm))
         (d (mapcar #'sum g)) 
         (ybar (/ (mapcar 
                   #'(lambda (z)
                       (sum (* (elt g z) criterium)))
                   mm)
                  d))
         (xbar (mapcar 
                #'(lambda (x)
                    (/ (mapcar 
                        #'(lambda (z)
                            (sum (* (elt g z) x)))
                        mm) 
                       d)) 
                predictors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "Group Sizes                    ~%")
    (format t "~a~%" rr)
    (dolist (i d)
            (format t "~4d" i))
    (format t "~%")
    (format t "~a~%" rr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "Group Means                    ~%")
    (format t "~a~%" rr)
    (format t "Criterion                      ~%")
    (dolist (i ybar)
            (format t "~10,6f~%" i))
    (format t "Predictors                     ~%")
    (dolist (i xbar)
            (dolist (j i)
                    (format t "~10,6f" j))
                    (format t "~%"))
    (format t "~a~%" rr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "Within Group Dispersions       ~%")
    (format t "~a~%" rr)
    (dotimes (i m)
             (let ((k (which (= indicator i))))
               (covariance-matrix-from-list
                (append (list (select criterium k))
                              (mapcar 
                               #'(lambda (z)
                                   (select z k)) predictors)))
               )
    (format t "~a~%" rr)
             )
    (format t "~a~%" rr)
    (format t "~|~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (format t "~a~%" rr)
    (format t "Dispersion Decomposition       ~%")
    (format t "~a~%" rr)
    (format t "Total                          ~%")
    (covariance-matrix-from-list
     (append (list criterium) predictors))
    (format t "Between                        ~%")
    (covariance-matrix-from-list
     (append (list ybar) xbar) :weights d)
    (format t "Within                         ~%")
    (covariance-matrix-from-list
     (append (list (- criterium (select ybar indicator)))
             (- predictors 
                         (mapcar #'(lambda (z)
                                     (select z indicator)) xbar))
                      ))
    (format t "~a~%" rr)
    (format t "~|~%")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (dotimes (z m)
             (format t "~a~%" rr)
             (format t "Total regression for group ~3d~%" z)
             (format t "~a~%" rr)
             (format t "Null Model                     ~%")
             (let* ((k (which (= indicator z)))
                    (l (length k))
                    (e (repeat 1 l))
                    (y (select criterium k)))
               (regression-model e y :intercept nil)
               (format t "Full Model                     ~%")
               (regression-model 
                (mapcar
                 #'(lambda (x) (select x k)) predictors)
                (select criterium k))
               (format t "~a~%" rr)
               )
             )
    (format t "~|~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "Total regression all groups    ~%")
    (format t "~a~%" rr)
    (format t "Null Model                     ~%")
    (regression-model (repeat 1 (length criterium)) criterium
                      :intercept nil)
    (format t "Full Model                     ~%")
    (regression-model predictors criterium)
    (format t "~a~%" rr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "Between-group regression       ~%")
    (format t "~a~%" rr)
    (format t "Null Model                     ~%")
    (regression-model (repeat 1 (length ybar)) ybar
                      :weights d :intercept nil)
    (format t "Full Model                     ~%")
    (regression-model xbar ybar :weights d)
    (format t "~a~%" rr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "Within-group regression        ~%")
    (format t "~a~%" rr)
    (format t "Null Model                     ~%")
    (regression-model (repeat 1 (length criterium))
                      (- criterium (select ybar indicator))
                    :intercept nil)  
    (format t "Full Model                     ~%")
    (regression-model (- predictors 
                         (mapcar #'(lambda (z)
                                     (select z indicator)) xbar))
                      (- criterium (select ybar indicator)))   
    (format t "~a~%" rr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "Contextual Analysis            ~%")
    (format t "~a~%" rr)
    (format t "Null Model                     ~%")
    (regression-model (repeat 1 (length criterium)) criterium
                      :intercept nil)
    (format t "Full Model                     ~%")
    (regression-model (append predictors
                         (mapcar #'(lambda (z)
                                     (select z indicator)) xbar))
                      criterium)   
    (format t "~a~%" rr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "Cronbach Model                ~%")
    (format t "~a~%" rr)
    (format t "Null Model                     ~%")
    (regression-model (repeat 1 (length criterium)) criterium
                      :intercept nil)
    (format t "Full Model                     ~%")
    (regression-model (append (- predictors 
                         (mapcar #'(lambda (z)
                                     (select z indicator)) xbar))
                         (mapcar #'(lambda (z)
                                     (select z indicator)) xbar))
                      criterium)   
    (format t "~a~%" rr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format t "~a~%" rr)
    (format t "ANCOVA                        ~%")
    (format t "~a~%" rr)
    (format t "Null Model                     ~%")
    (regression-model g criterium :intercept nil)
    (format t "Full Model                     ~%")
    (regression-model (append g predictors)
                                   criterium :intercept nil)    
    (format t "~a~%" rr)
    )           
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dispersion utility
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun covariance-matrix-from-list 
  (data &key (weights (repeat 1 (length (first data)))))
  (setf weights (/ weights (sum weights)))
  (dolist (i data)
      (dolist (j data)
          (format t "~10,6f"
              (sum (* weights (- i (sum (* weights i)))
                  (- j (sum (* weights j)))))))
           (format t "~%"))
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The example starts here
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def studpred (list
"sex"
"race"
"time spent on math homework"
"schooltype"
"ses"
"parents education"
"class structure"
"school size"
"urbanicity"
"geographic region"
"percent minority"
"student-teacher ratio"
))

(def predcats (list
	(list 
		(list 1 "male") 
		(list 2 "female"))
	(list 
		(list 1 "Asian or Pacific Islander")         
	    (list 2 "Hispanic, regardless of race")
	  	(list 3 "Black, not of Hispanic origin")
	  	(list 4 "White, not of Hispanic origin")
	  	(list 5 "American Indian or Alaskan Native")
	  	(list 8 "MISSING"))
	(list
	    (list 0 "None")          
	  	(list 1 "Less than 1 hour")
	  	(list 2 "1 hour")
	  	(list 3 "2 hours")
	  	(list 4 "3 hours")
	  	(list 5 "4-6 hours")
	  	(list 6 "7-9 hours")
	  	(list 7 "10 or more")
	  	(list 96 "MULTIPLE RESPONSE")
	  	(list 98 "MISSING"))
	(list
	  	(list 1 "Public school")
	  	(list 2 "Catholic school")
	  	(list 3 "Private, Other Religious Affiliation")
	  	(list 4 "Private, No Religious Affiliation"))
	(list nil)
	(list
	  	(list 1 "Did not finish H.S")
	  	(list 2 "H.S. grad or GED")
	  	(list 3 "GT H.S. & LT 4yr degree")
	  	(list 4 "College graduate")
	  	(list 5 "M.A. or equivalent")
	  	(list 6 "Ph.D., M.D., other")
	  	(list 7 "Don't know")
	  	(list 8 "MISSING"))
	(list
	  	(list 1 "Not at all accurate")
	  	(list 2 "")
	  	(list 3 "")
	  	(list 4 "")
	  	(list 5 "Very much accurate")         
  		(list 8 "MISSING"))
  	(list
  	 	(list 1 "1-199 students")
 		(list 2 "200-399")
 		(list 3 "400-599")
 		(list 4 "600-799")
 		(list 5 "800-999")
 		(list 6 "1000-1199")
 		(list 7 "1200+"))
 	(list
 	 	(list 1 "Urban")
 		(list 2 "Suburban")
 		(list 3 "Rural"))
 	(list
 	  	(list 1 "NORTHEAST")
  		(list 2 "NORTH CENTRAL")
  		(list 3 "SOUTH")
  		(list 4 "WEST")
  		(list 8 "MISSING"))
  	(list
  		(list 0 "None")
  		(list 1 "1-5%")
		(list 2 "6-10%")
		(list 3 "11-20%")
		(list 4 "21-40%")
		(list 5 "41-60%")
		(list 6 "61-90%")
		(list 7 "91-100%")
		(list 8 "MISSING"))
	(list nil)
		))

(def schoolind '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 
4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 
6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 
6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 
7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 
8 8 8 8 8 8 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9))

(def studdep '(
48.18 48.1 52.89 41.84 
42.51 57.45 32.93 64.3 36.17 56.2 47.73 47.81 44.2 34.99 50.16 36.17 46.55 
41.47 42.21 48.98 38.53 45.38 47.81 58.41 43.02 49.21 42.06 35.29 38.46 
34.99 42.36 40.89 31.46 40.81 31.53 64.52 50.24 37.28 43.68 43.54 40.81 
38.53 36.17 64.52 61.8 44.27 63.27 43.17 67.76 50.16 57.16 52.52 47.73 
52.81 46.7 36.47 52.45 70.04 51.41 66.88 43.83 70.04 32.64 70.04 53.7 37.35 
40.74 31.31 39.49 47.81 34.48 32.86 46.63 39.63 31.46 36.32 37.35 61.87 
40.81 40.81 37.42 43.17 40.08 41.99 64.23 53.7 65.33 45.3 47.73 61.94 32.71 
53.04 43.02 53.7 59.59 58.34 48.91 49.06 52.52 56.05 57.53 45.75 31.53 
59.59 47.81 46.92 52.45 41.84 58.34 40.81 42.51 41.99 45.52 43.17 45.6 
46.78 49.35 46.63 44.35 39.49 40.81 51.49 45.45 56.64 47.37 42.21 46.7 
46.92 51.41 52.52 44.35 61.94 67.69 56.05 67.69 60.62 60.69 61.8 59.51 
67.69 63.2 71.22 60.69 59.74 68.87 70.04 65.63 62.98 63.05 66.51 62.09 
51.34 64.15 57.23 66.58 57.23 69.01 53.84 68.87 63.27 43.17 60.25 71.22 
63.12 55.98 65.41 68.87 61.06 70.04 56.05 60.69 59.59 64.52 64.23 65.41 
70.04 66.58 63.12 63.05 64.3 55.98 63.05 66.58 53.62 54.88 66.51 58.41 
57.23 62.76 66.51 70.04 68.87 67.69 51.64 63.05 55.24 68.87 67.76 68.87 
46.41 45.52 44.49 64.67 52.45 39.71 53.7 57.53 52.52 61.87 39.78 33.89 
40.81 45.89 41.18 59.59 50.24 34.99 65.33 41.84 47.81 45.01 47.73 61.72 
43.24 56.2 39.78 36.84 33.82 38.38 39.71 37.35 55.98 40.89 71.22 47.88 
41.92 40.81 59.59 42.28 44.27 67.76 50.31 43.24 43.98 37.28 66.58 37.28 
43.17 35.58 36.76 62.17 64.15 64.23 37.28 45.89 42.65 45.08 46.7 33.89 
52.74) 
)

(def studdat '(
(2 1 
1 1 2 2 2 1 2 2 2 1 1 2 2 2 2 2 2 1 2 1 2 2 1 1 1 2 1 1 1 1 1 1 2 1 2 1 1 2 
1 2 2 2 1 2 1 2 2 1 1 2 1 2 1 1 2 2 2 1 1 1 2 2 2 1 2 1 1 2 1 1 1 2 2 1 2 2 
1 2 2 2 2 1 1 2 1 1 1 2 2 2 1 2 1 1 2 2 1 1 1 1 1 2 1 1 2 1 1 2 2 1 2 2 2 1 
2 2 2 2 1 2 1 1 1 1 2 2 2 2 1 1 2 1 2 2 2 2 2 2 1 1 1 1 2 2 1 1 2 2 2 2 1 2 
1 1 2 1 2 2 2 2 1 1 1 2 1 2 2 1 1 1 2 1 1 1 1 2 1 1 1 2 2 1 1 1 2 1 1 2 2 1 
2 1 1 2 1 1 1 1 2 1 2 1 1 2 1 2 2 1 2 2 1 1 1 2 2 2 2 2 1 1 2 1 2 2 1 2 1 1 
2 1 2 1 2 1 2 1 2 2 2 1 2 2 1 1 1 2 2 1 1 2 1 1 2 1 1 1 1 2) 
(4 4 4 4 4 4 
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 4 
4 1 4 4 4 4 4 4 4 1 4 4 4 4 1 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
4 4 4 4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 2 2 2 2 2 2 2 
4 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 1 3 4 4 4 4 4 4 4 4 4 4 4 4 
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 4 4 4 1 4 4 1 4 4 1 4 2 4 4 4 4 4 4 4 
4 4 2 3 3 4 4 4 4 3 4 4 4 3 3 3 3 4 3 3 3 4 3 4 3 4 3 3 4 4 3 3 3 4 4 4 3 4 
4 4 3 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4) 
(1 0 0 1 2 1 5 1 1 2 
1 1 1 2 1 4 1 2 1 1 1 1 1 2 2 1 1 5 2 1 4 4 3 2 1 0 1 3 3 3 2 3 4 4 3 1 4 2 
4 1 2 1 1 2 1 0 1 3 2 2 1 3 1 2 1 1 1 1 0 1 1 0 1 0 1 2 1 5 1 1 1 1 2 1 5 4 
4 1 2 0 2 1 1 0 0 2 2 0 1 1 1 0 2 0 1 1 0 1 1 0 2 1 1 1 1 1 1 2 1 3 1 0 0 0 
1 2 1 3 1 1 1 4 5 5 5 2 2 3 1 2 0 5 1 2 5 6 1 2 1 2 5 4 4 1 4 1 4 1 5 5 1 5 
4 0 2 5 4 2 5 1 3 4 3 4 7 5 1 1 3 4 3 3 1 4 5 3 1 5 4 6 3 5 5 5 5 2 5 4 3 1 
2 1 4 1 1 2 3 2 3 1 2 1 2 1 2 3 1 5 3 1 1 2 4 0 1 2 1 1 1 1 2 1 1 3 0 1 1 2 
1 1 5 3 1 2 0 4 1 2 0 0 2 3 3 1 0 2 0 1 1 1) 
(1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 1 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
(-0.13 -0.39 -0.8 -0.72 -0.74 -0.58 
-0.83 -0.51 -0.56 0.21 -0.7 -0.19 -1.34 -1.58 -0.05 -0.5 -0.26 -0.94 -0.46 
-0.39 0.58 0.48 -0.7 -1.24 -1.54 -1.06 -0.02 -1.63 -0.39 -1.07 -0.02 0.11 
0.13 -1.67 -1.64 -1.01 -1.01 -1.16 -0.29 -1.29 0.03 -1.09 -1.1 0.98 0.24 
0.35 0.65 -0.03 0.39 -0.19 -0.76 -0.11 1.21 -1.85 -0.9 -0.87 1.33 1.48 
-0.76 0.63 -0.12 0.03 -0.12 1.05 0.29 -0.34 0.95 -1.67 -0.52 -0.45 -1.1 
-2.04 0.06 -0.07 -1.21 -1.96 -0.38 -0.48 -1.05 -1.54 -0.14 0.92 -1.28 -1.31 
-0.02 -0.6 -0.13 -0.19 -0.26 0.44 -0.75 -0.89 0.48 -0.19 -0.2 0.05 -0.43 
-0.14 0.3 -0.49 -0.71 -0.21 -0.21 1.08 -1.29 0.22 -0.56 -0.53 0.23 -0.67 
0.14 -0.06 -2.08 -1.25 -0.86 -0.9 -2.41 0.18 -1.41 -1.05 -1.35 -1.11 -1.35 
-0.53 -1.29 -1.92 -0.97 -0.33 -0.15 -2.23 -0.3 1.63 0.48 0.69 0.6 0.77 1.19 
1.17 1.12 1.85 0.75 1 1.8 1.43 -0.12 0.98 1 1.23 0.86 0.18 1.48 1.25 1.18 
0.64 1.13 1.82 1.34 0.68 1.11 1.4 0.79 1.61 1.03 1.38 1.28 0.56 1.03 1.3 
1.23 1.42 1.01 1.32 0.97 1.31 0.09 1.48 -0.35 0.4 0.86 0.85 1.09 1 0.26 
0.98 1.5 0.93 1.02 1.55 1.26 1.68 1.34 1.05 1.8 0.68 0.23 1.22 1.01 1.18 
0.81 -0.71 -0.54 0.01 0.9 0.97 -0.14 -0.13 -0.88 1.55 0.86 -0.55 -0.02 
-0.69 0.18 -0.92 -2.04 -0.99 -1.96 1.05 -0.35 0.76 -1.12 0.29 -0.99 -0.49 
-0.32 -0.29 -1.42 -2.04 -1.1 1.25 -0.56 0.71 -1.33 -0.12 -0.69 -0.66 -1.18 
0.82 -0.85 -0.39 -0.86 -0.41 -0.44 -0.19 -1.03 0.03 -0.72 -1.28 -0.84 -0.49 
-0.67 0.09 -0.19 -0.16 -0.56 -0.23 -1.11 -0.29 -1.19 0.45) 
(2 2 2 2 2 2 2 3 
2 3 2 3 2 1 2 3 3 1 3 3 3 3 3 3 2 1 3 1 2 1 3 4 3 1 1 3 3 1 3 1 3 2 1 4 3 4 
5 3 3 3 3 3 6 1 2 2 5 6 3 3 2 3 3 5 3 2 5 2 2 2 1 1 3 3 1 1 2 3 2 2 2 5 1 1 
3 2 3 2 3 3 3 2 4 3 3 3 3 3 3 3 3 3 2 5 2 3 3 1 3 1 3 3 1 3 1 1 1 3 1 1 3 2 
1 3 2 1 3 3 2 1 3 6 4 5 4 4 5 5 6 6 4 4 6 5 4 5 4 5 4 4 6 5 5 5 4 6 6 4 4 5 
6 6 5 5 6 4 5 5 6 6 5 6 5 4 3 5 3 3 5 4 5 4 3 5 6 5 6 6 6 6 6 5 6 4 3 6 5 5 
4 3 1 3 5 5 3 3 3 6 6 2 3 3 3 3 1 2 1 5 3 4 2 3 2 4 2 2 2 1 1 5 2 4 1 3 1 2 
1 6 2 2 2 3 2 3 2 4 3 1 2 2 2 3 4 3 3 3 1 3 1 4) 
(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 5 5 5 5 5 5 5 5 5 5 
5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4) 
(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 4 
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) 
(2 2 2 
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
(2 2 2 2 2 2 2 
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
(0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 6 6 6 6 6 6 6 6 6 6 6 6 6 6 
6 6 6 6 6 6 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 5 5 5 
5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 
5 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
(19 19 19 19 19 19 19 19 19 19 
19 19 19 19 19 19 19 19 19 19 19 19 19 18 18 18 18 18 18 18 18 18 18 18 18 
18 18 18 18 18 18 18 18 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 
14 14 14 14 14 14 14 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 
18 18 18 18 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 
12 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 10 10 10 10 
10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 
10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 
10 10 10 10 10 10 10 10 10 10 10 10 10 14 14 14 14 14 14 14 14 14 14 14 14 
14 14 14 14 14 14 14 14 14 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 
22 22 22 22 22 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This runs the example, with homework predicting math achievement
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(between-within-total (list (elt studdat 2)) studdep schoolind)