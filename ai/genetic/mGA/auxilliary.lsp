;;; -*- Mode:Common-Lisp; Base:10; Fonts:(cptfont) -*-

;============================FILE-AUX.LISP================================
;=This file contains the auxilliary operators that accomplish some of the=
;=low level and irregular functions                                      =
;=========================================================================

;========================GET_CHROMOSOME===================================  
;accesses the structure of a population member and returns the chromosome
(defun GET_CHROMOSOME (pop loc)     
  (population_member-chrom (aref pop loc)))   

;======================GET_FITNESS========================================
;accesses the structure of a population member and returns the fitness
(defun GET_FITNESS (pop loc)	  
  (population_member-fitness (aref pop loc)))  

;===================GET_SUBFUNCTION_LIST==================================
;accesses the structure of a population member and returns the subfunction
(defun GET_SUBFUNCTION_LIST (pop loc)	  ; value list
  (population_member-subf_list (aref pop loc))) 

;========================RND==============================================
(defun RND (lo hi)	  ;generates a random number from lo to hi
  (+ lo (random (+ (- hi lo) 1))))

;==============================COMBINATION================================
;creates a combination list of max_number items taken num_places @ a time
(defun COMBINATION (max_number num_places comb_list &aux exit)
  (setf max_number (1- max_number))	  ;adjust for list index 0 to n-1
  (setf num_places (1- num_places))	  ;instead of 1 to n
  (if (< (nth num_places comb_list) max_number) 
			  ;if low val (farthest right) is less than 
			  ;max_number then inrement by one
      (setf (nth num_places comb_list) (1+ (nth num_places comb_list))) 
      (progn		  ;otherwise
        (do ((j num_places (1- j)))	  ;check the rest of the positions 
	     ((or (< j 1) exit))
	  (if (< (nth (1- j) comb_list) 
		 (+ max_number (* -1 num_places) j -1)) 
			  ;look for one that can be inremented
	      (progn                                               
	        (setf (nth (1- j) comb_list) (1+ (nth (1- j) comb_list))) 
			  ;increment the next highest value that 
			  ;can be incremented
		(do ((s j (1+ s)))
		     ((> s num_places))	  
			  ;and set all following vals one 
			  ;higher than the value to its left
		  (setf (nth s comb_list) 
			(+ (nth (1- j) comb_list) s (- 1 j))) 
		  (setf exit 't)))	  ;then exit 
	      )))))

;========================RESET_POP========================================		   
;resets all population parameters to nil by recreating the structure
(defun RESET_POP (respop) 
  (do ((i 0 (1+ i)))
      ((>= i popsize))
    (setf (aref respop i) (make-population_member))))

;=======================FLIP==============================================
;simulates the flip of a weighted coin
(defun FLIP (probability &aux tbase)	  
  (setq tbase 1000000)             
  (>= (* probability tbase) (random tbase)))

;=====================COMPLEMENT_BIT======================================
(defun COMPLEMENT_BIT (bit &aux bit_ret)  ;changes a 1 to 0 or a 0 to 1
  (setq bit_ret 1)
  (cond
    ((= bit 1)
     (setq bit_ret 0)))
  bit_ret)

;========================SHUFFLE_POP======================================
;creates a shuffled array of size popsize 
(defun SHUFFLE_POP (shuffle &aux count num other) 
  (do ((i 0 (1+ i)))
      ((>= i popsize))
    (setf (aref shuffle i) i))
  (setf count (1- popsize))
  (do ((j 0 (1+ j)))
       ((>= j count))	  ;swap entire population around
    (setf num (RND j count))	  ;determine random swap position
    (setf other (aref shuffle num))	  
    (setf (aref shuffle num) (aref shuffle j))	  ;swap one to the other
    (setf (aref shuffle j) other))	  
			  ;and the other back to the original place
  shuffle)                          


;=====================SETUP_METER=========================================
;sets up meter to show how much of the pop has been processed by OBJFUBC
(defun  SETUP_METER ()          
  (format t "percent population processed~%")
  (format t "0%                  100% ~%" ))

;==========================N_ARY_COUNT====================================
;counts a list in n-ary mode where n is maximum value of count
(defun N_ARY_COUNT (n_ary_list maximum position &aux len) 
  (cond
			  ;if not at maximum then increment current position
    ((< (nth position n_ary_list) maximum)
     (setf (nth position n_ary_list) (1+ (nth position n_ary_list))))
			  ;if all positions are maxed zero out list
    ((= (list-length n_ary_list) (count maximum n_ary_list))
     (SET_LIST n_ary_list 0))
			  ;if current and above postion are maxed check next
			  ;with another call to this function
    ((and (>= (nth (1- position) n_ary_list) maximum) 
	  (>= (nth position n_ary_list) maximum))     
     (N_ARY_COUNT n_ary_list maximum (1- position)))
			  ;if just the position is maxed
    ((>= (nth position n_ary_list) maximum) 
     (setf len (list-length n_ary_list))  
			  ;set everything above to zero
     (do ((i position (1+ i)))     
	 ((>= i len))	  
       (setf (nth i n_ary_list) 0)) 
			  ;and increment next position
     (setf (nth (1- position) n_ary_list) 
	   (1+ (nth (1- position) n_ary_list))))))

;====================================CHOOSE===============================
(defun CHOOSE (n1 n2)	  ;statistical operator n1 choose n2
  (/ (factorial n1) (* (factorial n2) (factorial (- n1 n2))))) 

;===============================FACTORIAL=================================
(defun FACTORIAL (n &aux ret_val) ;standard stat factorial operator
  (setf ret_val 1)
  (do ((i 1 (1+ i)))
       ((> i n))
    (setf ret_val (* i ret_val)))
    ret_val)

;=========================SET_ASC_LIST====================================
;sets up an ascending list starting at start and ending with start+length
(defun SET_ASC_LIST (list start)
  (do ((i start (1+ i)))
       ((>= i (list-length list)))
    (setf (nth i list) i)))

;==============================SET_LIST===================================
;sets all the elements of a list to a value
(defun SET_LIST (list value)
  (do ((i 0 (1+ i)))
       ((>= i (list-length list)))
    (setf (nth i list) value)))


;==============================SET_ARRAY==============================   deb
; sets all elements of an array
(defun SET_ARRAY (array num value)
  (do ((i 0 (1+ i)))
      ((>= i num))
    (setf (aref array i) value)))
