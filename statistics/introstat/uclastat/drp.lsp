;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Degree of Reading Power data (Moore and McCabe, Table 7.2, page 534
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def drp-tc-data (concatenate 'list (repeat 1 21) (repeat 0 23)))
(def drp-oc-data (list 24 43 58 71 43 49 61 44 67 49 53 56 59 52 62 54 57 33 46 43 57 42 43 55 26 62 37 33 41 19 54 20 85 46 10 17 60 53 42 37 42 55 28 48))
(def drp-tc (send data-variable-proto :new drp-tc-data))
(def drp-oc (send data-variable-proto :new drp-oc-data))
(send drp-tc :title "Treatment")
(send drp-tc :legend "Treatment Variable")
(send drp-tc :legend "Outcome Variable")
(send drp-oc :title "Outcome")
(def drp (send data-multivariable-proto :new (list drp-tc drp-oc)))
(send drp :title "DRP")
(send drp :legend "An educator believes that new directed reading activities in the classroom will help elementary school pupils improve some
aspects of their reading ability. She arranges for a third grade class of 21 students to take part in these activities for an 8-week period. A control
classroom of 23 third graders follows the same curriculum without the
activities. At the end of the 8 weeks, all students are given an Degree of
Reading Power (DRP) test, which measures the aspects of reading ability
that the treatment is designed to improve")
(edit-variable drp)
