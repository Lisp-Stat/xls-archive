;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number of earthquakes >= 7.0 this century
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEF quakenum-data (QUOTE (13 14 8 10 16 26 32 27 18 32 36 24 22 23 22 18 25 21 21 14 8 11 14 23 18 17 19 20 22 19 13 26 13 14 22 24 21 22 26 21 23 24 27 41 31 27 35 26 28 36 39 21 17 22 17 19 15 34 10 15 22 18 15 20 15 22 19 16 30 27 29 23 20 16 21 21 25 16 18 15 18 14 10 15 8 15 6 11 8 7)))
(setf quakenums (send data-variable-proto :new quakenum-data 
                      :title "Big Ones" 
                      :case-labels (to-string-list (+ 1900 (iseq 90)))
                      :legend 
"NUMBER OF EARTHQUAKES PER YEAR MAGNITUDE 7.0 OR GREATER 1900 - 1989.
Statistics were compiled from the Earthquake Data Base System of the
U.S. Geological Survey, National Earthquake Information Center, Golden CO         "
))
