(read-time)
(do ((i 0))
    ((> i 10000) t)

    (let* ((a (+ 1 2))
	   (b (+ a 1)))

      (+ a b))
    (setq i (1+ i))
    )
(printf "time after let loop: " (read-time))

(do ((i 0))
    ((> i 10000) t)

    (setq a (+ 1 2))
    (setq b (+ a 1))
    (+ a b)
    (setq i (1+ i))
    )

(printf "time after setq loop: " (read-time))
