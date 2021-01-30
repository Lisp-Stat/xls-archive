;
; This program demonstrates the use of dynamic graphics
; to choose an appropriate cut-point for coding a continuous
; covariate into a binary one.
;
; The program first calls gen-cut-data, which creates
; time, stat, and covar. The dynamic graph is created by a
; call to choose-cut-plot.
;
; gen-cut-data generates the data as follows. First, 
; gen-expo-data is used to generate two data sets of
; exponentially distributed variables. Each set has 50
; elements and no censoring. Set 1 has lambda=1 and set 2
; has lambda=3. A covariate is generated for each data set.
; For set 1, the covariate is uniform (0,3). For set 2, the
; covariate is uniform (3,4). Thus, the ideal cut-point should
; be 3. The two data sets are then merged and sorted by time.
;
; ENA - 6/5/91.
;

(load "get-init-cut")
(load "gen-cut-data")
(load "copy-choose-cut-plot")
(load "read-col")

(def can nil)

(get-init-cut)

(if (eql can nil)
    (list (cond ((= datype 0) (read-col filena))
            ((= datype 1) (gen-cut-data)))

          (setf cut1 (copy-choose-cut-plot time1 stat1 covar))))
