;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                  ;;
;;;;   Code to plot the quantiles of a shorter list against the       ;;
;;;;   emperical quantiles of a longer (or same length) list.         ;;
;;;;                                                                  ;;
;;;;   Questions and comments to Jason Bond                           ;;
;;;;                             (jbond@laplace.stat.ucla.edu         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun emp-qq-plot (x y &key (title "Empirical QQ Plot")
                 (variable-labels (list "Var 1" "Var 2")) case-labels)
                                  
  (let* (
         (nx (length x))
         (ny (length y))
         (nmin (min nx ny))
         (quant (/ (iseq nmin) nmin))
         (x1 (if (= nx nmin) (sort-data x) (sort-data y)))
         (x2 (if (= nx nmin) (quantile y quant) (quantile x quant)))
         (qqplot (send scatterplot-proto :new 2 :variable-labels
                   (if (= nx nmin) (list (concatenate 'string "Emp Quant of " 
                                           (first (reverse variable-labels)))
                                         (second (reverse variable-labels)))
                                   (list (concatenate 'string "Emp Quant of "
                                           (first variable-labels)) 
                                         (second variable-labels)))
                                         :title title))
        )
    (send qqplot :range 0 10 5)
    (send qqplot :add-points (list x2 x1))
    (send qqplot :x-axis t t 5)
    (send qqplot :y-axis t t 5)

    (if case-labels (send qqplot :point-label (iseq nmin) case-labels))
    (send qqplot :adjust-to-data)
    qqplot
  )
)

