;;;Demo program for FEDF scatterplot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notebook data
;;;
(def notebook-data
     '("Aero_486"           	0 245 232 1 1699 0
      "AMS_4533"            	2 266 211 2 2495 0
      "AST_Bravo"           	2 214 215 2 2748 1
      "AST_power"           	3 218 349 3 4758 3
      "CAF_486DX3/C"        	2 229 304 1 3259 0
      "Chaplet_486SX/25TFT" 	3 229 155 3 3399 1
      "Compaq_LTE4/25e"     	1 221 706 2 4339 3
      "CompUSA_486/33"      	0 251 230 3 2728 0
      "Epson_Action"        	0 140 232 2 1699 1
      "Hyperdata_DLC"   	0 282 252 3 2295 1
      "IBM_350" 		2 241 307 3 2800 1
      "KingTech_486" 		0 120 130 0 1695 1
      "Micro_win" 		0 191 220 1 1599 1
      "Micro_HCP65681c" 	2 392 233 2 3535 1
      "NEC_Versa33c" 		3 306 255 3 5538 3
      "Sager_NP" 		3 299 252 2 3750 3
      "Samsung_3800" 		0 148 230 1 2978 1
      "TI_DX/25" 		0 248 446 2 2678 1
      "Toshiba_1900C" 		2 203 317 3 2700 1
      "Twinhead_DX/33" 		3 274 254 1 3895 2
      "Zenith_425Lnc" 		3 234 314 2 3999 2
      "Apple_power_165c" 	2 277 240 3 2339 1
      "Apple_power_180c" 	3 276 219 3 4079 3
      "Acer_750c" 		3 222 428 3 3988 2
      "Amrel_486" 		3 413 130 1 5249 2
      "AMS_5366a" 		3 339 252 2 3895 2
      "AMS_7525a" 		3 220 258 3 3290 1
      "Aspen_Aura33" 		2 288 139 1 2595 1
      "AST_Bravo_NB" 		0 215 316 2 1948 2
      "CAF_486DX3" 		0 280 434 1 2559 0
      "CAF_486DX3/T" 		3 280 316 1 4650 2
      "CAF_486SLC" 		0 174 247 1 2099 0
      "Chaplet_486SX/25" 	0 230 239 3 1999 1
      "Compaq_Contura4/25" 	0 221 355 1 2949 1
      "Compaq_LTE4/33c" 	3 271 220 2 5419 2
      "CompuAdd_425TX" 		0 229 410 3 2090 1
      "CompuAdd_425TXT" 	3 232 324 3 3890 1
      "CompUSA_486DX2/66TFT" 	3 350 217 1 4728 2
      "CompUSA_486DX2/66" 	0 335 407 3 3228 1
      "Ergo_486" 		0 217 316 3 3795 2
      "Ergo_PowerBrick" 	3 312 239 2 4295 2
      "HyperData_SLC/33" 	2 183 204 3 3395 1
      "HyperData_DX2/50" 	3 333 119 3 5495 1
      "IBM_720c" 		3 270 400 3 4900 1
      "Jetta_486DX33" 		0 234 255 0 1995 1
      "Micro-HCP65681m" 	0 384 332 2 2805 2
      "Micro_HCP65681t" 	3 396 238 2 4305 2
      "Mitsuba_486SX" 		3 187 233 2 3895 2
      "NEC_Versa25c" 		3 252 240 3 4888 3
      "Noteable_N425" 		0 147 216 0 1886 1
      "Primax_450v" 		0 215 201 1 2640 1
      "TI_EDX2/50" 		3 362 236 2 5278 3
      "TI_DX2/50" 		0 350 401 2 3499 1
      "TI_SX/25" 		2 228 223 2 3278 1
      "Toshiba_T4600c" 		3 269 352 3 4699 1
      "Twinhead_DX/33m" 	0 251 252 1 2195 1
      "Twinhead_DX2/66t" 	3 341 238 1 4399 2
      "Xinetron_486SLC25" 	0 151 232 1 1580 1
      "Zenith_425Lno" 		0 234 527 2 2599 1
      "Zenith_425Lnp" 		2 234 337 2 2999 0
      "Apple_180" 		1 313 302 3 3749 2
))
            
(def notebook (transpose (split-list notebook-data 7)))

(let* ((x (nth 3 notebook))
       (tx (floor (/ x 100)))
       (bl (+ tx (/ (- x (* tx 100)) 60))))
  (setf (nth 3 notebook) bl))


(def data 
     (list (elt notebook 4) (elt notebook 1) (elt notebook 6)
           (elt notebook 5) (elt notebook 2) (elt notebook 3)))

(def labels (elt notebook 0))

(def var-labels 
     (list "ease-of-use" "display-type" "screen-quality"
           "price" "speed" "battery`-life" ))

(def title "notebook PC")

(def category '(0 1 2))

(def cat-list
     '((0 ("bad" "fair" "good" "excel"))
       (1 ("mono" "m-act" "color" "c-act"))
       (2 ("bad" "fair" "good" "excel"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;sample run with notebook data
;;;FEDF scatterplot
(fedf-scatter data :var-labels var-labels :title title)