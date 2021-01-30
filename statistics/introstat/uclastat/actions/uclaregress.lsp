(defmeth data-multivariable-proto :regress ()
(if (= 1 (length (send self :data)))
    (message-dialog "You can't do regression with a single variable")))
