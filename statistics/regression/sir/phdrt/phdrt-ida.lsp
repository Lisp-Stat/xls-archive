(setf spm nil)
(setf gt nil)
(setf pca nil)
(setf hg nil)
(setf phd nil)
(setf phd-p nil)
(def data(cons y x))
(def p(length x))

(def IDA-option (car (choose-subset-dialog "which IDA tool(s) to be used"
                      '("scatterplot-matrix" "Grand Tour"
                        "Pricipal Component Analysis" "Histogram"
                        "Pricipal Hessian Directions Analysis")
                        :initial '(0 1 2 3 4) )))

(when (member 0 IDA-option)
      (def spm (scatterplot-matrix
                (select data (iseq (min 10 (+ 1 p))))
                :location (list 0 36)
                :size (list 300 300)
                :title data-file
;               :variable-labels (select varnames )
                ))
      (send spm :linked t))

(when (member 1 IDA-option)
      (def gt (tour-plot x
                :title "Grand Toyr"
                :location '(318 36) 
                :size '(280 160)))
;               :variable-labels (select varnames )
       (send gt :linked t)
       (send gt :axis-rotate)
       (send gt :mouse-mode 'hand-rotate))

(when (member 2 IDA-option)
      (def pca (spin-plot (pca-plot x)
                :title "PCA"
                :variable-labels (list "pc1" "pc2" "pc3")
                :location '(318 310) :size '(280 160)))
      (send pca :linked t)
      (send pca :axis-rotate)
      (send pca :mouse-mode 'hand-rotate))

(when (member 3 IDA-option)
      (def hg (histogram y
                :title "histogram y"
                :location '(618 36) :size '(280 160)))
       (send hg :linked t ))

(when (member 4 IDA-option)
      (require "phd-model")
      (setf res (car (reg x y)))
      (setf phd (phd-model x res))
      (def phd-p(spin-plot (list (send phd :phd1) res (send phd :phd2))
                :title "pHd"
                :variable-labels (list "phd1" "res" "phd2")
                :location '(618 310) :size '(280 160)))
    (send phd-p :linked t)
    (send phd-p :axis-rotate)
    (send phd-p :mouse-mode 'hand-rotate))

(def plot-list (list spm gt pca hg phd-p))

(def color-list (repeat (list (nth 2 *colors*) (nth 5 *colors*) 
                              (nth 3 *colors*) (nth 7 *colors*)
                              (nth 6 *colors*) (nth 4 *colors*) 
                              (nth 0 *colors*)) 4))

(dolist (j IDA-option)
        (when (nth j plot-list)
              (send (nth j plot-list) :use-color t)
              ))

(dolist (j IDA-option)
        (when (nth j plot-list)
              (send (nth j plot-list) :redraw)
              ))

(send CAP-menu-item :enabled T)
