;;; -*- Mode:Common-Lisp; Base:10; Fonts:(cptfont) -*-

;============================FILE-OPS.LISP================================
;=This file contains all the mGA genetic operators.                      = 
;=========================================================================

;======================CUT================================================
;takes chromosome cuts it & returns the 2 pieces
(defun CUT (chromosome &aux rd len) 
  (setf len (list-length chromosome));cut position is random
  (if (> len 1)
      (setf rd (RND 1 (1- len))))
  (cond
    ((<= len 1) (list chromosome '()))
    ((list (subseq chromosome 0 rd) (subseq chromosome rd len)))))

;===============================SPLICE====================================
;takes 2 pieces puts them together and returns the result
(defun SPLICE (chrom1 chrom2)  
  (append chrom1 chrom2))
;===============================MUTATE====================================
;changes a single bits value 1 to 0 or vice versa at a rate of probability
(defun MUTATE (gene probability)
  (cond
    ((FLIP probability) (setf gene (COMPLEMENT_BIT gene)))) 
  gene)

;================================MUTATION=================================
;takes a chromosome and checks for a mutation at each bit
(defun MUTATION (chrom &aux len)  
  (cond
    ((not (zerop mut_prob))       
                          ;if mutation rate is not zero
     (setf len (list-length chrom))       
                          ;mutation is determined by mut_prob
     (do ((i 1 (1+ i)))
         ((> i len))      ;run through entire chrom
       (setf (nth 1 (nth (1- i) chrom)) 
             (MUTATE (nth 1 (nth (1- i) chrom)) mut_prob)))))
  chrom)                  ;return chrom

;=============================CUT_AND_STACK===============================
;takes 2 strings mate1 and mate2 cuts them and returns list of cut pieces
(defun CUT_AND_STACK (mate1 mate2 cut_prob &aux loc_stack)
  (cond
    ((FLIP (* cut_prob (list-length mate1)))  ;check for cut
     (setq mate1 (CUT mate1))
                          ;if so then cut mate1
     (push (cadr mate1) loc_stack)          
                          ;put second piece of mate1 on stack
     (setq mate1 (pop mate1))))   
                          ;set mate1 to first piece
  (cond
    ((FLIP (* cut_prob (list-length mate2)))   ;check for cut
     (progn
       (setq mate2 (CUT mate2))              
                          ;if so then cut mate2  
       (push (car mate2) loc_stack)    ; changed 06/26/91             deb
                          ;put second piece of mate2 on the stack
       (push (cadr mate2) loc_stack))) ; changed 06/26/91             deb
                          ;put first piece of mate2 on the stack
    ((push mate2 loc_stack)))   
                          ;otherwise put mate2 (uncut) onto the stack
  (push mate1 loc_stack) ;put mate1 (cut or uncut) onto the 
                          ;stack & return it
  loc_stack)              ; returns the stack  deb

;=============================CROSSOVER===================================
(defun CROSSOVER (mate1 mate2 cut_prob splice_prob &aux child1 child2 
                  child3 child4 cross)
  (setf stack (CUT_AND_STACK mate1 mate2 cut_prob)) 
                          ;generate stack of cut pieces
  (setf child1 (SPLICE_TESTER splice_prob ))      
                          ;check for splice on child1
  (setf child2 (SPLICE_TESTER splice_prob ))      
                          ;check for splice on child2
  (setf child3 (SPLICE_TESTER splice_prob ))      
                          ;check for splice on child3
  (setf child4 (SPLICE_TESTER splice_prob ))      
                          ;check for splice on child4
  (setf cross (list child1 child2 child3 child4 ))	    
                          ;return list of children
  (remove nil cross))


;========================SPLICE_TESTER====================================
;tests for splice between CUT_AND_STACK(ed) pieces list name -stack
;example stack (resembles) --> '((a b c) (d e f) (h i) (j k l))  execute-
;(SPLICE_TESTER 1.0) (100% prob) (a b c d e f) (top two members spliced)
;(SPLICE_TESTER 0) (0% prob)  (abc) (top member no splice)                                     
(defun SPLICE_TESTER (splice_prob)
  (cond			  ;make sure stack is not empty
    (stack (cond  
	      ((FLIP splice_prob)     
			  ;if splice then splice two pieces of stack
	       (SPLICE (pop stack) (pop stack)))
	      ((pop stack))))	  ;otherwise return top member of stack
    ))

;===========================INIT_SELECTION================================
;compares successive shuffled population members amd returns the fittest 
(defun INIT_SELECTION (&aux first second fittest)
  (if (>= pick popsize)   ;if array has been gone through just reset 
      (setf pick 0))      ;the pointer to zero
  (setf first pick)       ;get the first 
  (setf second (1+ pick)) ;get the second
  (setf pick (+ pick 2))  ;inrement location +2
  (if (>= (GET_FITNESS oldpop first) 
          (GET_FITNESS oldpop second))  ;compare fitnesses
      (setf fittest first)              ;& return fittest         
      (setf fittest second))
  fittest)

;=====================DET_SELECTION=======================================
;controls type of selection routine to be used
(defun DET_SELECTION (&aux fittest)   
  (if (<= loopvar init_select_gen)      ; changed                        deb
      (setf fittest (INIT_SELECTION))   
                          ;use INIT_SELECTION for first three generations
      (progn
        (if (equal thres nil)  ;check thres flag
            (setf fittest (NORM_SELECTION shuffle))  
                          ;thres flag is not set use normal selection
            (setf fittest (THRESH_SELECTION)))))
                          ;if it is set use thres selection
  fittest)

;============================NORM_SELECTION===============================
(defun NORM_SELECTION (shuffle &aux first second fittest)
  (if (>= pick (1- popsize)) 
      (progn 
        (setf pick 0)     ;when you reach the end of the array then,
        (SHUFFLE_POP shuffle)))   ;re-shuffle it
  (setf first (aref shuffle pick))   ;select two guys from it
  (setf second (aref shuffle (1+ pick)))  
  (setf pick (+ pick 2))  ;increment pointer
  (if (< (GET_FITNESS oldpop first) (GET_FITNESS oldpop second))  
                          ;compare the fitnesses of the two
      (setf fittest second)
      (setf fittest first))
  fittest)                ;return the fittest

;=========================THRESH_SELECTION================================
(defun THRESH_SELECTION (&aux first second chrom1 chrom2 fittest threshold
                         flag stop_position position pts_alike length1 length2)
  (if (>= pick (- popsize shufnum))       
                          ;if there's not enough room at the end of the pop
      (progn              ;to run through shufnum# members, then reset the 
        (setf pick 0)     ;pick and the shuffle array
        (SHUFFLE_POP shuffle)))
  (setf stop_position (+ pick shufnum))
  (setf first (aref shuffle pick))
  (setf fittest first)
  (setf chrom1 (copy-list (GET_CHROMOSOME oldpop first)))      
                          ;get chrom with removed duplicates
  (setf chrom1 (remove-duplicates chrom1 :key 'car :from-end 't)) 
  (setf length1 (list-length chrom1))
  (setf position (1+ pick))
  (do ((i 1 (1+ i)))((or flag (>=  position stop_position))) 
    (setf position (+ i pick))                      
                          ;exit if thres(hold) is reached or if shufnum 
                          ;members have been looked at
    (setf second (aref shuffle position))        
    (setf chrom2 (copy-list (GET_CHROMOSOME oldpop second)))  
                          ;get chrom with removed duplicates
    (setf chrom2 (remove-duplicates chrom2 :key 'car :from-end 't))  
    (setf length2 (list-length chrom2))
    (setf threshold (/ (* length1 length2) shufnum))	    
                          ;calculate threshold value
    (setf pts_alike (list-length (intersection chrom1 chrom2 :key 'car)))
    (cond ((>= pts_alike threshold)       
                          ;calculate the # of common points
           (progn         ;if # of common pts > threshold value
             (setf flag 't)
             (if (>= (GET_FITNESS oldpop first) (GET_FITNESS oldpop second))  
                          ;compare fitnesses                 
                 (setf fittest first)     ;& return most fit member 
                 (setf fittest second))     
             (setf (aref shuffle (+ pick i)) (aref shuffle (1+ pick)))    
                          ;swap the two positions
             (setf (aref shuffle (1+ pick)) second)))))    
  (setf pick (+ pick 2))  ;increment pointer
  fittest)                ;return the fittest


