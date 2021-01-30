(defun classification (x y varnames n p k cat)
  (let*(
        (auto-inter (choose-item-dialog
                     "Which Tree Construction Mode Do You Want To Use?"
                     '("Automatic." "Interactive." "Semi-Automatic.")
                     ))
        )
    (when (= auto-inter 0)
          (def sirct-out (classification-auto x y varnames n p k cat))
          )
    (when (= auto-inter 1)
          (def sirct-out (classification-inter x y varnames n p k cat))
          )
    ))
