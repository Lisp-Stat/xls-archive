(def weight (+ 180 (* 20 (normal-rand 10))))
(def height (+ 70 (* 5 (normal-rand 10))))
(def age (sample (iseq 20 60) 10 t))
(def iq (sample (iseq 50 100) 10 t))
(def income (+ 10000 (* 1000 (beta-rand 10 10 10))))
(def name (list "Jan" "Don" "Rob" "Ker-Chao" "Tom" "Bob" "Tony" 
                "Larry" "Linda" "Dick"))
(show-data (list height weight age iq income) 
           :varnames (list "Height" "Weight" "Age" "Iq" "Income")
           :case-labels name)

;;  The Author is not responsible for any of the outcomes of any of 
;;  the variables.




