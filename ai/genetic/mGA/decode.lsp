;;; -*- Mode:Common-Lisp; Base:10; Fonts:(cptfont) -*-

;=========================FILE-DECODE.LISP================================
;=This file contains functions responsible for decoding a raw chromosome =
;=thereby generating a processed chromosome                              =
;=========================================================================

;=======================EXTRACT===========================================
;EXTRACT takes a position/value list mGA coding and puts the bits in their 
;proper positions   example-- (EXTRACT ((2 0)(1 1)(3 1)....)) 
;                    would return     (nil 1 0 1...)
(defun EXTRACT (chrom &aux ret_list pos val pos_val no_duplicate length)
  (setf ret_list (make-list bits_per_chrom))	  
			  ;setup list to copy bits into
  (setf no_duplicate (remove-duplicates chrom :key 'car :from-end 't))
			  ;remove overspecified positions
  (setf length (list-length no_duplicate))                             
			  ;get # of positions to be set
  (do ((i 0 (1+ i)))
       ((>= i length))
    (setf pos_val (nth i no_duplicate))
    (setf pos (nth 0 pos_val))	  ;get position
    (setf val  (nth 1 pos_val))	  ;get value
    (setf (nth pos ret_list) val))	  
			  ;set ret_list pos(ition) to val(ue)
  ret_list)

;===================FILL_NIL_POSITIONS====================================
;fills unspecified positions with std_fill array vals
(defun FILL_NIL_POSITIONS (list1)
  (do ((pos 0 (1+ pos)))
       ((>= pos (list-length list1)))
    (if (equal (nth pos list1) nil)	  
			  ;look for positions w/ nil value
        (setf (nth pos list1) (nth pos std_fill))))	  
			  ;& fill them w/ std_fill array
  list1)

