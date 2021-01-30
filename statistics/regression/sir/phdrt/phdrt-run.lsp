;(send *listener* :location 0 17)
;(send *listener* :size 400 300)
(defun phd-rt(x y)
  (let*(
        (auto-inter (choose-item-dialog
                     "Which Tree Construction Mode Do You Want To Use?"
                     '("Automatic." "Interactive.")
                     ))
        )
    (when (= auto-inter 0)
           (time (setf out(regression-tree-auto x y)))
          )
    (when (= auto-inter 1)
           (time (setf out(regression-tree-int x y)))
          )
    out))

(def phdrt-out(phd-rt x y))
(send CAP-menu-item :enabled T)
