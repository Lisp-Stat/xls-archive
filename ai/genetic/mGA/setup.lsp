;;; -*- Mode:Common-Lisp; Base:10; Fonts:(cptfont) -*-

;============================FILE-SETUP.LISP==============================
;=This file contains the code for setting up all the parameters od the  =
;=mGA via a user specified setup file. The routines are primarily        =
;=involved with accesssing the file, pattern matching and the setting    =
;=the specified parameter. The exeption is MAKE_NEW_POP (see below)      =
;=========================================================================
;========================MAKE_NEW_POP=====================================
(defun MAKE_NEW_POP(num_positions max_length &optional copies &aux count
		    bit_positions position_list bit_list bits max_size)
  (format t "~%Making Population of building block size ~A" num_positions)
  (format t "~%and chromosome length ~A" max_length)
  (cond ((not copies)
	 (setf copies 1)))
  (setf bit_positions (expt 2 num_positions))	  
			  ;calc bit position counter
  (setf max_size 
	(round (* copies (* bit_positions (CHOOSE max_length
num_positions)))))
			  ;calc pop size for blocks
  (setf newpop (make-array max_size))	  
			  ;create newpop array
  (setf position_list (make-list num_positions))  
			  ;create position list
  (SET_ASC_LIST position_list 0)  
			  ;initialize to (0 1 2 ...num_positions-1) 
  (setf bit_list (make-list num_positions))	  ;create bit_list
  (setf count 0)
  ;  (SET_LIST bit_list 0)   ;reset bit_list to all zeros (initially)  commented
  ;                                                                       deb
  (do ()
      ((>= count (1- max_size)))  ;generate every block
      (SET_LIST bit_list 0)   ;reset bit_list to all zeros (initially)    deb
    (do ((j 0 (1+ j)))	  ;run through all bit combinations  
	 ((>= j bit_positions))
      (setf bits nil)	  ;reset temporary bit/value holder
      (cond
	((/= j 0)	  ;on second and subseq passes increment bit_list
	 (N_ARY_COUNT bit_list 1 (1- num_positions))))	 
      (do ((k 0 (1+ k)))
	  ((>= k num_positions)) ;create pos/value list  
        (push (list (nth k position_list) (nth k bit_list)) bits))
      (do ((m 0 (1+ m)))
	  ((>= m copies)) ;make copies if specified
	(setf (aref newpop count) (make-population_member))  
			  ;create structure population_memeber
	(setf (population_member-chrom 
		(aref newpop count)) (reverse bits))	  
			  ;set member to the bit/val list
	(setf count (1+ count)))
      )
    (COMBINATION max_length num_positions  position_list ))   
			  ;increment to next position
  (format t "~%Population Size is now ~A~%" count)
  (setf popsize count))	  ;return population size created



;===============================CUT_CALC==================================
(defun CUT_CALC (&aux doping_phase cut_phase)
  (setf cut_phase (truncate (log (/ popsize final_popsize) 2))) 
			  ;calc ammount of cutting that needs to be done
  (setf doping_phase (truncate (- (log (1- popsize) 2) 
				  (log (1- (* 2 num_subfunctions))2))))
			  ;calc ammount of gen for proper doping of pop
   (cond 
    ((< (* 2 cut_phase) doping_phase)	  ;case 1
     (setf cutpopgen (* 2 cut_phase))	  ;enough space for cutting
     (setf cut_every doping_phase)	  ;so set cutpopgen to val
     (setf cookgen doping_phase))
    ((>= (* 2 cut_phase) doping_phase)	  ;case 2
     (setf cutpopgen doping_phase)	  ;not enough space for cutting
     (setf cut_every (* 2 (- doping_phase cut_phase))) ;set cut_every to
     (setf cookgen doping_phase)  ;proper value
     )))

;----------->>>>>>>>>LOAD PROGRAM STARTS HERE<<<<<<<<<<<<<<---------------
;the purpose is to setup all the global variables given a setup file

;===============================SETUP_GA==================================
(defun SETUP_GA ()
  (setf first_time 1)
  (fresh-line)
  (format t "enter name of setup file--") ;
  (setf setup_file (read))	  ;get name of setup file from user
  (fresh-line)
  (setf instream (open setup_file :direction :input))
			  ;open setup file for reading
  (setf indata (read instream))	  ;read from setup file
  (do () ((equal indata 'end))	  ;read until end marker is reached
    (if (equal indata 'OBJ)	  ;if OBJ marker is reached go to SETUP_OBJ
        (SETUP_OBJ)) 
    (if (equal indata 'POPULATION)	  ;population setup marker
        (SETUP_POP))
    (setf indata (read instream))
    )
  (close instream)	  ;close setup file 
  (format t "setup file ~A is loaded~%" setup_file))  
			  ;inform that setup file has been loaded

;============================SETUP_OBJ====================================
;sets up objective function parameters
(defun SETUP_OBJ (&aux inlist table# scale subfunction)
  (setf subfunction 0)
  (setf num_tables 0)
  (setf bit_spec (make-list num_subfunctions))	  
			  ;create bit specifier array
  (format t "setting up objective function~%")	  
			  ;tell user what's being set up
  (setf indata (read instream))	  ;read from file
  (do () ((equal indata '***))	  ;read until *** patterm is found
    (if (equal indata 'subfunction_bits)  ;if setup for subfunction bits
        (progn
	  (setf inlist nil)	  ;reset all parameters
	  (setf table# nil)
	  (setf scale nil)
	  (setf indata (read instream))	  ;read next data item from file
	  (do () ((not (numberp indata))) ;read until a # is not encountered
	    (push indata inlist)  
			  ;make it into a list of numbers 
			  ;(the subfunction bits)
	    (setf indata (read instream)))	  ;read next
	  (if (equal indata 'ltable)	  ;if lookup table label then,
	      (progn 
	        (setf indata (read instream))	  ;read next value
		(setf table# indata)	  ;get lookup table number 
		(if (> table# num_tables) ;keep track of # of lookup tables 
		    (setf num_tables table#))
		(setf indata (read instream))	  ;read next file data
		))
	  (if (equal indata 'scale)	  ;if scale factor
	      (progn
	        (setf indata (read instream))	  ;read in data 
		(setf scale indata)))	  ;assign it to scale
			  ;now that all data has been gotten for 1 
			  ;subfunction setup bit_spec structure for it
	  (setf (nth subfunction bit_spec) 
		(make-bit_cluster :bit_specifier (reverse inlist) 
				  :table_specifier table#
				  :scale_factor scale))
	  (fresh-line)
	  (setf subfunction (1+ subfunction))))	  
			  ;increment subfunction counter
	  
    (if (equal indata 'table)	  
			  ;if lookup table then go to lookup table setup routine
        (SETUP_TABLE))
    (if (equal indata 'template)  
			  ;if template then go to template setup section
        (SETUP_TEMPLATE))
    (setf indata (read instream)) ;read next data
    ))

;====================SETUP_TEMPLATE=======================================
;sets up competitive template
(defun SETUP_TEMPLATE ()  ;
  (setf std_fill (make-list bits_per_chrom))	  
			  ;setup size of standard fill array
  (do ((i 0 (1+ i)))
       ((>= i bits_per_chrom))	  ;get all the bits
    (setf (nth i std_fill) (read instream))))	  ;set the bits

;=====================SETUP_TABLE==========================================
;sets up lookup table made of two parts-bits and coresponding value
(defun SETUP_TABLE (&aux table_number inlist data_value)
  (format t "setting up lookup table~%") 
  (cond
    (first_time		  ;if the first time through then create 
			  ;the lookup table
     (setf lookup_table (make-list (1+ num_tables))) 
     (setf first_time nil)))
  (setf table_number (read instream))	  ;get table number
  (setf indata (read instream))	          ;read next file data
  (do () ((equal indata 'end_table))	  ;read until the end of that table
    (setf inlist nil)
    (setf data_value nil)         ;get data value from file
    (if (equal indata 'bvalue)	  ;if bit value
        (progn
	  (setf indata (read instream))
	  (do () ((not (numberp indata)))
	    (push indata inlist)
	    (setf indata (read instream)))	  ;assign to bit list
	  (if (equal indata 'dvalue)	          ;if data value
	      (progn
	        (setf indata (read instream))
		(setf data_value indata)  ;assign data value
		(setf indata (read instream))))
	  (push  (list (reverse inlist)  data_value)
		 (nth table_number lookup_table)) 
			  ;put bits & coresponding value on lookup table
	  ))))

;=============================SETUP_POP====================================
;this routine sets up population parameters
(defun SETUP_POP ()
  (format t "setting up population parameters~%")
   (setf bits_per_chrom nil)
  (setf maxgen nil)
  (setf num_subfunctions nil)
  (setf cookgen nil)
  (setf shufnum nil)
  (setf seed nil)
  (setf cutpopgen nil)
  (setf cut_prob 0)
  (setf splice_prob 0)
  (setf mut_prob nil)
  (setf pspe nil)
  (setf thres nil)
  (setf garbage_collect 1)
  (setf bldg_blk_size nil)
  (setf member_copies 1)
  (setf init_select_gen 1)
  (setf indata (read instream))
  (do () ((equal indata '***))	  ;read until star pattern is encountered
    (cond ((equal indata 'chrom_length)
	   (setf indata (read instream))
	   (setf bits_per_chrom indata))
	  ((equal indata 'maxgen)
	   (setf indata (read instream))
	   (setf maxgen indata))
	  ((equal indata 'num_subfunctions)
	   (setf indata (read instream))
	   (setf num_subfunctions indata))
	  ((equal indata 'shufnum)
	   (setf indata (read instream))
	   (setf shufnum indata))
	  ((equal indata 'seed)
	   (setf indata (read instream))
	   (setf seed indata))
	  ((equal indata 'cut_prob)
	  (setf indata (read instream))
	  (setf cut_prob indata))
	  ((equal indata 'splice_prob)
	   (setf indata (read instream))
	   (setf splice_prob indata))   
	  ((equal indata 'mut_prob)
	   (setf indata (read instream))
	   (setf mut_prob indata))
	  ((equal indata 'pspe)
	   (setf indata (read instream))
	   (setf pspe indata))
	  ((equal indata 'thres)
	   (setf indata (read instream))
	   (setf thres indata))
	  ((equal indata 'bldg_blk_size)
	   (setf indata (read instream))
	   (setf bldg_blk_size indata))
	  ((equal indata 'init_select_gen)
	   (setf indata (read instream))
	   (setf init_select_gen indata))
	  ((equal indata 'final_popsize)
	   (setf indata (read instream))
	   (setf final_popsize indata))
	  ((equal indata 'member_copies)
	   (setf indata (read instream))
	   (setf member_copies indata))
	  ((equal indata 'garbage_collect)
	   (setf indata (read instream))
	   (setf garbage_collect indata)))
    (setf indata (read instream))))       ;get next data




