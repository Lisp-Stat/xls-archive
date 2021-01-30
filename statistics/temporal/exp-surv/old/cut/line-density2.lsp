(provide 'line-density2)

(defun LINE-DENSITY2 (x beginindex endindex)    ;x values of some line
  (let* ((denx (first (kernel-dens x)))
         (len (length denx))
         (inc (floor (/ (- endindex beginindex) len)))
         (step 0)
         (dens1 (first (kernel-dens x)))
         (maxd (max dens1))
         (mind (min dens1))
         (inx (/ (- maxd mind) 5)))

  (dolist (i denx)
          (if (and (< i (+ inx mind)) (>= i mind))
              (def key 1))
          (if (and (< i (+ inx inx mind)) (>= i (+ inx mind)))
              (def key 2))
          (if (and (< i (+ inx inx inx mind)) (>= i (+ inx inx mind)))
              (def key 3))
          (if (and (< i (+ inx inx inx inx mind)) (>= i (+ inx inx inx mind)))
              (def key 4))
          (if (and (< i maxd) (>= i (+ inx inx inx inx mind)))
              (def key 5))
      
          (case key
            (1 (send plot :linestart-width (+ step beginindex) 1))
            (2 (send plot :linestart-width (+ step beginindex) 2))
            (3 (send plot :linestart-width (+ step beginindex) 3))
            (4 (send plot :linestart-width (+ step beginindex) 4))
            (5 (send plot :linestart-width (+ step beginindex) 5)))
           
          (setf step (+ step inc)))))
