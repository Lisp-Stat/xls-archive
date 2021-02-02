(new-section "Tutorial" "tutorial.scr")

(load "abrasion")
(load "precip")
(load "cholesterol")

(define-section-item "Linked"
  (let ((h (histogram hardness :title "Hardness"))
        (p (plot-points tensile-strength abrasion-loss 
                        :variable-labels 
                        (list "Tensile Strength" "Abrasion Loss")))
        (n (name-list 30)))
    (send h :linked t)
    (send p :linked t)
    (send n :linked t)))

(define-section-item "Spin"
  (spin-plot (list hardness tensile-strength abrasion-loss)
             :title "Abrasion Loss Data"
             :variable-labels
             (list "Hardness" "Tensile Strength" "Abrasion Loss")))

(define-section-item "Power"
  (let ((sx (sort-data abrasion-loss))
        (ns (normal-quant (/ (iseq 1 30) 31))))
    (flet ((bc (x p)
             (let* ((bcx (if (< (abs p) .0001)
                             (log x)
                             (/ (^ x p) p)))
                    (max (max bcx))
                    (min (min bcx)))
               (/ (- bcx min) (- max min)))))
      (let* ((pl (plot-points ns (bc sx 1)))
             (s (interval-slider-dialog
                 '(-1 2) :action
                 #'(lambda (p)
                     (send pl :clear-points :draw nil)
                     (send pl :add-points ns (bc sx p))))))
        (send pl :add-subordinate s)
        (send s :value 1)))))
