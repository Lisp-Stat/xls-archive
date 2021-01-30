;;; -*- Mode:Common-Lisp; Base:10; Fonts:(cptfont) -*-

;========================FILE-OBJFUNC.LISP================================
;=This file contains functions related to subfunction look-up, sacaling  =
;=and summation as well as setting the overall member fitnesses          =
;=========================================================================

;==================OBJFUNC================================================
;is responsible for accessing and decoding population members fitness via
;decode operators and set the overall population member's fitness
(defun OBJFUNC (&optional calc_val_flag &aux chromosome step)
  (RESET_STAT_INFO)
  (format t "~%~% OBJFUNC/STATISTICS ")  ;let user know whether 
  (if calc_val_flag
      (format t "(set fitness & update stats)~%") 
			  ;long lookup table is being used or
      (format t "(update stats only)~%"))         
			  ;short no lookup table is being used
  (SETUP_METER)		  ;sets up to display ammount of pop processed
  (setf step (round (/ (float popsize) 20)))	  
			  ;setup disp meter stepsize-meter is 20 chars wide
  (do ((i 0 (1+ i)))
       ((>= i popsize))
    (if (= (rem i step) 0)	  ;if even multiple of step, then
        (princ "*"))	  ;print a mark for 1/20th of population processed
    (if calc_val_flag	  ;this flag is set if decode is to be used
        (progn
	  (setf chromosome (GET_CHROMOSOME newpop i))
	  (setf chromosome (EXTRACT chromosome))
	  (if (not pspe)  ;is pspe flag is not set then
	      (setf chromosome (FILL_NIL_POSITIONS chromosome))) 
			  ;fill in unspecified positions w/ std_fill array 
	  (setf chromosome (SUBFUNCTION_DECODE chromosome)) 
			  ;get bits & decode info for subfunctions
	  (setf chromosome (SET_SUBFUNC_VALUE chromosome)) 
			  ;generate subfunction value list
	  (setf (population_member-subf_list (aref newpop i)) chromosome)) 
			  ;save subfunction list as part of chromosome
	(progn		  ;if value does not need to be calculated
	  (setf chromosome (GET_SUBFUNCTION_LIST newpop i))	  
			  ;get just update the statistics
	  ))
    (setf (population_member-fitness (aref newpop i)) 
	  (eval (push '+ chromosome))) 
			  ;calc overall fitness (add up all subfunc vals)
    (UPDATE_STATS i))	  ;& update stats with them
  (fresh-line))

;==========================SET_SUBFUNC_VALUE=============================
;table lookup for list of subfunctions the list resembles the following 
;((lookup_table# scale factor (subfunc bits))(lookup_table#.....())....())
(defun SET_SUBFUNC_VALUE (sub_info_list &aux subf_string subfunct_table 
			  scale_factor subfunct_table# ret_val_list 
			  num_of_decode)  
  (setf num_of_decode (list-length sub_info_list))
			  ;get # of subfunctions to decode
  (setf ret_val_list (make-list num_of_decode))   
			  ;setup list to be returned of subfunction 
			  ;decoded values		  
  (do ((subf_num 0 (1+ subf_num)))                
      ((>= subf_num num_of_decode))	  ;run through all the input list        
    (setf subfunct_table# (nth 0 (nth subf_num sub_info_list)))	  
			  ;get lookup table#
    (setf scale_factor (nth 1 (nth subf_num sub_info_list))) 
			  ;get lookup table#
    (setf subf_string (nth 2 (nth subf_num sub_info_list))) 
			  ;get string to be matched
    (setf subfunct_table (nth subfunct_table# lookup_table))    
			  ;access the lookup table specified by subfunct_table# 
    (do ((i 0 (1+ i)))  
	 ((>= i (list-length subfunct_table)))  
			  ;go through all the lookup table entries
      (if (equal (nth 0 (nth i subfunct_table)) subf_string) 
			  ;check for a match of decoded lists
	  (setf (nth subf_num ret_val_list)	  
			  ;if there is a match set the subfunction to 
			  ;the coresponding table value
		(* (nth 1 (nth i subfunct_table)) scale_factor)))
      (if (equal (nth subf_num ret_val_list) nil)
	  (setf (nth subf_num ret_val_list) 0))))   
			  ;no match in table means string not fully specified
  ret_val_list)		  ;or not specified in table, so fitness is zero

;=====================SUBFUNCTION_DECODE==================================
;given the bit list will decode from the lookup table the subfunction 
;values final list resembles-- 
;((lookup_table#,scale factor,(subf_bit_list),(....),(....)......(....))
(defun SUBFUNCTION_DECODE (bit_list &aux subf_info scale_factor 
			   bit_specifier table_specifier temp_list 
			   bit_value subf_information)
  (setf subf_information '())	  ;clear list
  (do ((subf_num 0 (1+ subf_num)))	  
			  ;bit_spec specifies subfunction bits, scale factor
			  ;& lookup table for each subfunction
       ((>= subf_num (list-length bit_spec)))	  
    (setf subf_info (nth subf_num bit_spec))	  
			  ;get single subfunction specifier off bit_spec
    (setf scale_factor (bit_cluster-scale_factor subf_info))    
			  ;get scale factor
    (setf bit_specifier (bit_cluster-bit_specifier subf_info))	  
			  ;get bit specifier (position numbers)
    (setf table_specifier (bit_cluster-table_specifier subf_info))	  
			  ;get table specifier
    (setf temp_list '())
    (do ((i 0 (1+ i)))	  ;get all specified bits & put them in a list
	 ((>= i (list-length bit_specifier)))
      (setf bit_value (nth i bit_specifier))	  ;get bit value
      (push (nth bit_value bit_list) temp_list))  ;put it on list
    (setf temp_list (list (reverse temp_list)))    
    (push scale_factor temp_list)         ;put scale factor on list
    (push table_specifier temp_list)	  ;and table specifier
    (push temp_list subf_information))	  
			  ;put all this on the subfunction information list
  (reverse subf_information)) 




