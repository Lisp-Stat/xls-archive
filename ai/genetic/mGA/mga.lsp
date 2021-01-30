;;; -*- Mode:Common-Lisp; Base:10; Fonts:(cptfont) -*-

     
;============================FILE-MGA.LISP================================
;=This file contains the global variable declarations, the mGA function, =
;=and important phase coordination functions                             =
;=========================================================================
(defstruct population_member chrom fitness subf_list)	  
			  ;structure used for each member of a population

(defstruct bit_cluster table_specifier scale_factor bit_specifier)
			  ;structure used for bit specifier decoder
(defvar avg_fitness)      ;statistical variable which specifies the 
                          ;population average fitness
(defvar bit_spec)         ;specifies for each subfunction bits,scale
                          ;factor and lookup table
(defvar bits_per_chrom)   ;specifies the length of decoded chromosome
(defvar bldg_blk_size)	  ;set in setup file is used to generate initial 
                          ;population
(defvar cookgen)          ;the # of generations in the primordial phase
(defvar cut_prob)         ;prob that a chrom will be cut into two pieces
                          ;by CUT
(defvar cutpopgen)        ;the # of generations after which the pop will  
                          ;be cut in half every other generation
(defvar cut_every)	  ;the # of generations after which the pop will
			  ;be cut in half every generation
(defvar data)             ;stream to output data for plotting or analysis
(defvar final_popsize)		  ;the final population size we will end up with
(defvar first_time)       ;flag used by SETUP_GA to mark first time 
                          ;through setup routine
(defvar garbage_collect)  ;set number of generation between garbage 
                          ;collections
(defvar indata)           ;temp variable used to read from file 
                          ;(see SETUP_GA)
(defvar init_select_gen)  ;# of generations to use INIT_SELECTION
(defvar instream)         ;a stream used to read from the setup file
(defvar lookup_table)     ;specifies the bit strings and their 
                          ;coresponding decoded values
(defvar loopvar)          ;is the generation counter
(defvar max_numgood)      ;statistical variable which keeps track of 
                          ;max # of optimum building blocks
(defvar maxfitness)       ;statistical variable which holds the maximum 
                          ;decoded chrom fitness
(defvar maxgen)           ;the total # of generations that will be executed
(defvar member_copies)    ;# of building block copies to be made in initial
                          ;population
(defvar minfitness)       ;statistical variable which holds the maximum 
                          ;decoded chrom fitness
(defvar mut_prob)         ;probability that s single bit will be mutated 
                          ;by MUTATION
(defvar newpop)           ;array of structure population_member 
(defvar num_subfunctions) ;specifies the # of subfunctions
(defvar num_tables)       ;specifies the # of lookup tables
(defvar numgood)          ;stat variable which keeps track of the total # of 
                          ;(111) building blocks
(defvar oldpop)           ;array of structure population_member   
(defvar pick)             ;postion marker for selection routines 
                          ;THRESH_SELECT,NORM_SELECTION & DET_SELECT
(defvar popsize)          ;the size of the population
(defvar pspe)             ;flag for partial string partial evaluation
(defvar screen&file)      ;output stream for both screen and file
(defvar seed)             ;random # generator seed
(defvar setup_file)       ;name of the file that the setup parameters 
                          ;are read from
(defvar shuffle)          ;shuffle array used in selection routines
(defvar shufnum)          ;max # of pop members that will be looked at in 
                          ;THRESH_SELECT
(defvar splice_prob)      ;prob that two chroms will be spliced together 
                          ;by SPLICE
(defvar stack)            ;global stack for subsequent calls to stack
(defvar stat)             ;output statistical file path vairable 
(defvar std_fill)         ;array used for filling in unspecified positions 
                          ;during chrom decode
(defvar subf_pos_max)     ;list used to count optimal building blocks 
(defvar sumfitness)       ;stat variable which holds the sum of all the 
                          ;fitnesses in the pop
(defvar thres)            ;is a flag when set true the program will use 
                          ;THRESH_SELECT for selection
(defvar vertnum_spot)     ;stat counter that countes the # of optimal 
                          ;building blocks for each subfunction position

;=====================mGA=================================================
;this is the main control structure of the program that is in charge
;of overall program flow
(defun mGA ( &aux max_gene)
  (format t "MGA~%")
 ; (setq w:more-processing-global-enable nil) 
			  ;keeps **MORE from waiting for key hit
			  ; TI-Explorer specific command               deb
  (setq stat (open "statout.dat" :direction :output)) 
                          ;open data file for stats
  (setq screen&file *standard-output*) 
; (make-broadcast-stream *standard-output* stat))  
                          ;sets up output screen & file streams
  (setq data (open "data.dat" :direction :output));sets up data file 
  (setf loopvar 0)         
  (SETUP_GA)              ;program which loads setup file                     
  (setf subf_pos_max (SETUP_SUBFUNC_MAX)) ;setf up subfunction maxim
			  ;for statistics optimium bldg block counting 
  ; (setq *random-state* (system:random-create-array 71. 35. seed))
                          ;seed random # generator  
			  ; TI-Explorer specific command                deb
  (setf max_gene (1- bits_per_chrom))	  ;set array max number
  (MAKE_NEW_POP bldg_blk_size bits_per_chrom member_copies)
                          ;generate every useful building block
                          ;of size building_blk_size
  (CUT_CALC)		  ;calculate cutting and phase parameters
  (INITIAL_OUTPUT)        ;output initial information
  (OBJFUNC t)             ;calculate initial objective function vals
  (STATISTICS)            ;print out statistics
  (do ((gen_count 1 (1+ gen_count)))	  ;generation loop
       ((> gen_count maxgen))
    (setf pick 0)	  ;reset shuffle array pointer
    (setf loopvar gen_count)	  ;sets global loopvar to local gen_count
    (cond 
      ((= (rem gen_count garbage_collect) 0)
       (format t "collecting garbage~%")	  ;collect garbage
       ; (gc-immediately :silent t)    ; TI-Explorer specific function   deb
       (gc)                         ; KCL specific function           deb
       (format t "finished collecting garbage~%")))
    (cond
      ((<= gen_count cookgen)          
                          ;check for primordial phase or juxtapos phase
       (setf shuffle (make-array popsize))	  ;create shuffle array
       (SHUFFLE_POP shuffle)	  ;initialize shuffle array
       (PRIMORDIAL)	  ;execute primordial phase
       (STATISTICS)	  ;print out statistics
       (cond 
	 ((and (<= gen_count cutpopgen) (or (>= gen_count cut_every) 
					    (evenp gen_count)))   
			  ;start cutting population in half from 2nd 
			  ;generation  up to and including cutpopgen
	  (setf popsize (round (/ (float popsize) 2))))))         
			  ;cut population in half 
      (t		  ;default juxtapostional
       (setf shuffle (make-array popsize))        ;create shuffle array  deb
       (SHUFFLE_POP shuffle)	  ;initialize shuffle array
       (JUXTAPOSITIONAL)	  ;execute juxtapositional phase
       (STATISTICS)))) ;print out statistics
  (close data)
  (close stat))		  ;close the statistical and data files

;==========================PRIMORDIAL=====================================
;uses deterministic selection to reproduce a new pop (no cut or splice)
(defun PRIMORDIAL (&aux position step)
  (format t "~%~%PRIMORDIAL~%")	  ;print out wht phase the program is in
  ; (setf oldpop (copy newpop))     ; TI-Explorer specific command       deb
  (setf oldpop (copy-tree newpop))  ;copy over all pop info to oldpop    deb
  (setf newpop (make-array popsize))
  (SETUP_METER)
  (setf step (round (/ (float popsize) 20)))	  
			  ;setup disp meter stepsize-meter is 20 chars wide
  (do ((i 0 (1+ i)))
      ((>= i popsize))
    (if (= (rem i step) 0)	  ;if even multiple of step, then
        (princ "*"))	  ;print a mark for 1/20th of population processed
    (setq position (DET_SELECTION))	  ;select individual
    (setf (aref newpop i) (aref oldpop position))) 
			  ;set newpop to the individual
  (OBJFUNC))		  ;call short objective function

;===========================JUXTAPOSITIONAL===============================
;uses deterministic selection and cut and splice to produce new population
(defun JUXTAPOSITIONAL (&aux position1 position2 mate1 mate2 cross count 
			num_of_children step)
  (format t "~%~%JUXTAPOSITIONAL~%")  ;print out what phase the program is in
  ; (setf oldpop (copy newpop))    ; TI-Explorer specific command          deb
  (setf oldpop (copy-tree newpop))  ; copy newpop to oldpop              deb
  (setf newpop (make-array popsize))  ; reset newpop                     deb
  (SETUP_METER)
  (setf step (round (/ (float popsize) 20)))	  
			  ;setup disp meter stepsize-meter is 20 chars wide 
  ; (RESET_POP newpop)	  ;reset shuffle array     commented             deb
  (setf count 0)	  ;reset counter
  (do ()		  ;add # of children each time through the loop 
      ((>= count popsize))
    (setf position1 (DET_SELECTION))	  ;select position1
    (setf position2 (DET_SELECTION))	  ;select position2
    (setq mate1 (GET_CHROMOSOME oldpop position1))	  
			  ;get chromosome of mate1@ position1    
    (setq mate2 (GET_CHROMOSOME oldpop position2)) 
			  ;get chromosome of mate2 @ position2   
    (setf cross (CROSSOVER mate1 mate2 cut_prob splice_prob))	  
			  ;perform crossover of the two
    (setf num_of_children (list-length cross))	  
			  ;get # of children from resulting cross
    (do ((k 0 (1+ k)))
	 ((or (>= k num_of_children) (>= count popsize)))	  
			  ;copy over children to newpop
      (setf (aref newpop count) (make-population_member))  ; added        deb
      (setf (population_member-chrom (aref newpop count)) 
	    (MUTATION (nth k cross)))
      (if (= (rem count step) 0)	  ;if even multiple of step, then
	  (princ "*"))	  ;print a mark for 1/20th of population processed  
      (setf count (1+ count))))
  (OBJFUNC t))		  ;use long objectve function-lookup table for 
			  ;new chromosomes formed
 
