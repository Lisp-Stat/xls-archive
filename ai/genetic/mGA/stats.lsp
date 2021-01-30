;;; -*- Mode:Common-Lisp; Base:10; Fonts:(cptfont) -*-

;============================FILE-STATS.LISP==============================
;=This file contains the statistical and reporting related code          =
;=========================================================================

;=================INITIAL_OUTPUT==========================================
;writes out initial data to data file, statistics file and screen
(defun INITIAL_OUTPUT ()
  (format screen&file "~3%RUN PARAMETERS~%")
  (format screen&file "Maxgen = ~A~%" maxgen)
  (format screen&file "Cookgen = ~A~%"cookgen)
  (format screen&file "Cutpopgen = ~A~%" cutpopgen)
  (format screen&file "Cut_every = ~A~%" cut_every)
  (format screen&file "final_popsize = ~A~%" final_popsize)
  (format screen&file "Init_select_gen = ~A~%" init_select_gen)
  (format screen&file "Threshold = ~A~%" thres)
  (format screen&file "Shuffle-Down Number = ~A~%" shufnum)
  (format screen&file "Random Seed = ~A~%"  seed)
  (format screen&file "pspe = ~A~%" pspe)
  (format screen&file "Competitive Template- ~A~3%" std_fill)
  (format data "Generation Popsize Maximum_Fitness Average_Fitness~%"))

;=====================UPDATE_STATS========================================
(defun UPDATE_STATS (pop_member &aux good_count subf_list total_fitness)
  (setf total_fitness 0)  ;reset counter variables
  (setf good_count 0)
  (setf subf_list (GET_SUBFUNCTION_LIST newpop pop_member))
  (setf total_fitness (GET_FITNESS newpop pop_member))
  (do ((j 0 (1+ j)))
       ((>= j (list-length subf_list)))	  
			  ;run through all the subfunctions for the subf_list
    (if (>= (nth j subf_list) (- (nth j subf_pos_max) .01)) 
			  ;check for max subfunctions
        (progn
	  (setf good_count (1+ good_count)) 
			  ;local counter to keep track of max # of optimal
			  ;subfunctions in single chrom
	  (setf (aref vertnum_spot j) (1+ (aref vertnum_spot j))) 
			  ;keeps track of max# of optimal subfunctions on
			  ;a per subfunction basis
	  (setf numgood (1+ numgood)))))        
			  ;count total number of optimal subfunctions
  (setf sumfitness (+ sumfitness total_fitness)) 
			  ;keep track of the total sum fitness
  (if (> total_fitness (nth 0 maxfitness))                   
			  ;keep track of max fitness in population
      (progn
	(setf (nth 0 maxfitness) total_fitness)
	(setf (nth 1 maxfitness) pop_member)
	))
  (if (< total_fitness minfitness)                    
      (setf minfitness total_fitness))
			  ;keep track of min fitness in population
  (if (> good_count max_numgood)                
			  ;keep track of max# of optimal subfunctions
			  ;in single population member
      (setf max_numgood good_count)))
 
;==================RESET_STAT_INFO========================================
(defun RESET_STAT_INFO () ;resets all statistical counters 
  (setf vertnum_spot (make-array num_subfunctions))
  (setf maxfitness (make-list 2))
  (setf (nth 0 maxfitness) 0)	  ;maximum fitness in the population
  (setf (nth 1 maxfitness) 1)
  (setf minfitness subf_pos_max)
  (setf minfitness (eval (push '+ minfitness)))
			  ;minimum fitness in the population 
  (setf sumfitness 0)	  ;total fitness of population
  (setf numgood 0)	  ;total number of optimal optimal subfunctions 
			  ;in population
  (setf max_numgood 0)	  ;max optimal subfunctions in a single chromosome
  ; (array-initialize vertnum_spot 0))   ; TI-Explorer specific function  deb
  (SET_ARRAY vertnum_spot num_subfunctions 0))   ; created (in aux.lisp)  deb
			  ;number of optimal subfunctions in each 
			  ;subfunction position

;===========================STATISTICS====================================
;prints out population statistics  to file and screen each generation
(defun STATISTICS ()
  (format t "~%~%running STATISTICS~%")  
  (setf avg_fitness (float (/ sumfitness popsize)))    
			  ;calc average fitness
  (format screen&file "Generation# ~A~%" loopvar)   
			  ;print out various statistics
  (format screen&file "Popsize = ~A~%" popsize)
  (format screen&file "Minfitness = ~A~%" minfitness)
  (format screen&file "Maxfitness = ~A~%" (nth 0 maxfitness))
  (format screen&file "Average fitness = ~A~%" avg_fitness)
  (format screen&file "Maximum number of optimal subfunctions = ~A~%" 
	  max_numgood)
  (format screen&file "Total number of optimal subfunctions = ~A~%" 
	  numgood)
  (format screen&file "Average number of optimal subfunctions = ~A~%" 
	  (float (/ numgood popsize)))
  (format data "~A" loopvar)
  (format data " ~A ~A ~A ~%" popsize (nth 0 maxfitness) avg_fitness)
  (do ((i 0 (1+ i))) ;prints out information per subfunctional 
        ((>= i num_subfunctions))
	(format screen&file 
"Total and Average Number of optimal subfunctions in position ~A = ~A and ~A~%" 
		(1+ i) (aref vertnum_spot i) 
		(/ (float (aref vertnum_spot i)) popsize)))
  (format screen&file "~%Best solution so far -~A~%" 
	  (FILL_NIL_POSITIONS	 
	    (EXTRACT (GET_CHROMOSOME newpop (nth 1 maxfitness)))))
  (format screen&file "Fitness = ~A~%" (nth 0 maxfitness)))

;==========================SETUP_SUBFUNC_MAX==============================
;this function sets up a list that has the maximum value each subfunction
;in that position can obtain (scale factor applied)
(defun SETUP_SUBFUNC_MAX (&aux ret_list subf_max subf_info 
			  scale_factor table_specifier subfunct_table)
  (setf ret_list '())	  ;initialize return list 
   (do ((i 0 (1+ i)))	  ;run through all the subfunctions
      ((>= i (list-length bit_spec)))
    (setf subf_max 0)	  ;set max to lowest possible val
    (setf subf_info (nth i bit_spec)) 
			  ;get subfunction specifier
    (setf scale_factor (bit_cluster-scale_factor subf_info))
			  ;get scale factor from list  
    (setf table_specifier (bit_cluster-table_specifier subf_info))
			  ;get lookup table specifier off list
    (setf subfunct_table (nth table_specifier lookup_table))
			  ;access the single lookup table
    (do ((j 0 (1+ j)))	  ;go through entire lookup table
	((>= j (list-length subfunct_table)))
			  ;and find the highest value in the table
      (if (> (nth 1 (nth j subfunct_table)) subf_max)
	  (setf subf_max (nth 1 (nth j subfunct_table)))))
    (push (* scale_factor subf_max) ret_list))
			  ;put it on the list
   (reverse ret_list))	  ;set returned lists to proper order







