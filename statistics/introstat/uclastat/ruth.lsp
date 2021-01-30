;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Home runs hit by Babe Ruth in his 15 years with the Yankees (1920-1934)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ruth-data 
 (to-string-list '(54 59 35 41 46 25 47 60 54 46 49 46 41 34 22)))
(def ruth-legend "Home runs hit by Babe Ruth in his 15 years with the Yankees, between 1920 and 1934")
(def ruth (send multi-variable-proto :new (list ruth-data)
                         :title "Babe Ruth"
                         :legend ruth-legend
                         :variable-labels '("Homeruns")
                         :case-labels (to-string-list (+ 1920 (iseq 15)))))
(send ruth :data-dialog)
