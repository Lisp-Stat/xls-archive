;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Newcomb - Michelson Measurements of the Speed of Light
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def newcomb-data '(
28 22 36 26 28 28 
26 24 32 30 27 24
33 21 36 32 31 25
24 25 28 36 27 32
34 30 25 26 26 25
-44 23 21 30 33 29
27 29 28 22 26 27
16 31 29 36 32 28
40 19 37 23 32 29
-2 24 25 27 24 16
29 20 28 27 39 23))
(def newcomb (send data-variable-proto :new newcomb-data :title "Newcomb"
:legend "The first reasonably accurate measurements of the speed of light were made a little over 100 years ago by A.A. Michelson and Simon Newcomb. These are 66 measurements made by Newcomb between July and September 1882. Newcomb measured how long light took to travel from his laboratory on the Potomac River to a mirror at the base of the Washington Monument and back, a total distance of about 7400 meters. He then coded the data by multiplying by 10**9, and subtracting 24800."))
(edit-variable newcomb)