(defun smooth(x y node terminal-nodes sub_node)
  (let*(
        (tau (nth 0 (get-value-dialog "choose your tau value"
                                      :initial '.2)))
        (l-terminal-nodes (length terminal-nodes))
        (nobs (length y))
         loop-case
         loop-nbr
         adjoin
         temp_w
        (case-weight(repeat (list nil) l-terminal-nodes))
         temp-t
         reg1
         loop-fit-y
         loop-case-order
         case-order
         new-y
         new-x
        )
   (dotimes (i l-terminal-nodes)
           (setf loop-case (send (nth (nth i terminal-nodes) node) :case))
           (setf l-loop-case(length loop-case))
           (setf w nil)
           (setf loop-w nil)
           (setf loop-proj-t nil)
           (setf loop-dir-t nil)
           (setf loop-ancestor-t nil)

     (dolist (j sub_node)
           (setf loop-proj nil)
           (setf loop-dir nil)
           (setf loop-ancestor nil)
           (setf loop-nbr(send (nth j node) :case-neighbor))
           (setf loop-ancestor(send (nth j node) :ancestor))
           (dolist (k (cons j loop-ancestor))
               (setf loop-proj(append loop-proj 
                                (list (send (nth k node) :phd-proj-var))))
               (setf loop-dir(append loop-dir 
                                (list (send (nth k node) :phd-evct))))
               )
       (dotimes (r 2)
           (setf adjoin(intersection loop-case (nth 1 (nth r loop-nbr))))
           (setf its-adjoin(car (nth r loop-nbr)))
           (unless (= (length adjoin) 0)
(print (list 'adjoin (length adjoin) adjoin))
              (setf temp_w(weight1 x adjoin its-adjoin loop-case 
                                   loop-proj loop-dir tau))
              (setf loop-w(append loop-w (list temp_w)))
              )
        )
      )
(print (list 'loop-w loop-w))
      (when (= (length loop-w) 1)
               (setf w (car loop-w)))
      (unless (= (length loop-w) 1)
               (setf w(repeat 0 l-loop-case))
               (dotimes (l (length loop-w))
                        (setf w(+ w (nth l loop-w)))
                        )
               )

      (setf loop-ancestor-t (send (nth (nth i terminal-nodes) node) :ancestor))
      (dolist (k loop-ancestor-t)
              (setf loop-proj-t(append loop-proj-t 
                                    (list (send (nth k node) :phd-proj-var))))
              (setf loop-dir-t(append loop-dir-t 
                                    (list (send (nth k node) :phd-evct))))
              )

      (setf temp-t(weight x loop-case loop-proj-t loop-dir-t tau))
      (setf (nth i case-weight) (/ temp-t (+ w temp-t)))
(print (list 'case-weight (length (nth i case-weight)) (nth i case-weight)))

      (setf reg1(regression-model (subset x loop-case) (select y loop-case) 
                                   :weights (nth i case-weight) :print nil))
      (setf loop-fit-y(append loop-fit-y (* (send reg1 :fit-values)
                                            (nth i case-weight))))
      (setf loop-case-order(append loop-case-order loop-case))
  )

      (setf loop-fit-x(select (transpose x) loop-case-order))
      (setf new-y(repeat 0 nobs))
      (setf new-x(repeat (list nil) nobs))
      (setf case-order(sub-loc (iseq nobs) loop-case-order))
      (setf (select new-y case-order) loop-fit-y)
      (setf (select new-x case-order) loop-fit-x)
      (setf new-x(transpose new-x))

      (when (= (length new-x) 2)
                 (def plot0(spin-plot (list (car x) y (nth 1 x))
                      :location '(618 310) :size '(280 160)))
                      (send plot0 :linked t)
                 (def plot (spin-plot (list (car new-x)
                                             new-y
                                            (nth 1 new-x))
                            :variable-labels (list "x1" "y" "x2")
                            :title "phd-smoothing-view "
                            :location '(618 310) :size '(280 160)))
                      (send plot :linked t)
                      (send plot :use-color t)
                      (send plot :mouse-mode 'hand-rotate)
                      (send plot :axis-rotate)
                 )
 ))

  (defun weight1(x adjoin its-adjoin loop-case loop-proj loop-dir tau)
     (let*(
           (l-loop-case(length loop-case))
           (l-loop-dir (length loop-dir))
           (nx (subset x its-adjoin))
           (nx1(subset x adjoin))
           (n  (length its-adjoin))
           (n1 (length adjoin))
            p1x
            p1x1
            loop-weight
           )
       (setf loop-weight(repeat 1 l-loop-case))
       (dotimes (i l-loop-dir)
           (setf temp_c_w(repeat 0 l-loop-case))
           (setf xx(transpose (select nx (nth i loop-proj))))
           (setf xx1(transpose (select nx1 (nth i loop-proj))))
           (setf m(length (nth i loop-proj)))
           (setf matx(make-array (list n m) :initial-contents xx))
           (setf matx1(make-array (list n1 m) :initial-contents xx1))
           (setf dir(cdr (nth i loop-dir)))
           (setf p1x(%* matx dir))
           (setf p1x1(%* matx1 dir))
           (setf temp_w(sub-weight1 (min p1x) (max p1x) p1x1 tau))
           (setf loc(sub-loc loop-case adjoin))
(print (list 'adjoin-loc loc))
           (setf (select temp_c_w (sub-loc loop-case adjoin)) temp_w)
           (setf loop-weight(* loop-weight temp_c_w))
           )
      loop-weight))

  (defun sub-weight1(a b px tau)
     (let*(
           (nob (length px))
           (d (* tau (- b a)))
           (d_1(repeat (- a d) nob))
           (d_2(repeat (+ b d) nob))
           (temp(+ (/ 1 (abs (- px d_1))) (/ 1 (abs (- px d_2)))))
           (h_t (exp (* (* d temp) -1)))
          )
     (setf out-case  (union (which (< px d_1)) (which (> px d_2))))
     (setf l-out-case(length out-case))
     (when (= l-out-case 1)
             (setf (select h_t (car out-case)) 0)
             )
     (unless (= l-out-case 0)
             (setf (select h_t out-case) (repeat 0 l-out-case)) 
             )
      h_t))

  (defun weight0(x interior loop-case loop-proj loop-dir tau)
     (let*(
           (l-loop-dir (length loop-dir))
            loop-weight
           (nx (subset x loop-case))
           (nx1(subset x interior))
           (n  (length loop-case))
           (n1 (length interior))
           (w-out(repeat 0 l-loop-dir))
            p1x
            p1x1
           )
       (setf loop-weight(repeat 1 n))
       (dotimes (i l-loop-dir)
           (setf xx(transpose (select nx (nth i loop-proj))))
           (setf xx1(transpose (select nx1 (nth i loop-proj))))
           (setf m(length (nth i loop-proj)))
           (setf matx(make-array (list n m) :initial-contents xx))
           (setf matx1(make-array (list n1 m) :initial-contents xx1))
           (setf dir(cdr (nth i loop-dir)))
           (setf p1x(%* matx dir))
           (setf p1x1(%* matx1 dir))
           (setf (nth i w-out) (car (sub-weight (min p1x1)(max p1x1) p1x tau)))
           (setf loop-weight(* loop-weight (nth i w-out)))
           )
      loop-weight))

  (defun weight(x loop-case loop-proj loop-dir tau)
     (let*(
           (n  (length loop-case))
           (l-loop-dir (length loop-dir))
           (nx (subset x loop-case))
            w-out
            p1x
            loop-weight
           )
       (setf loop-weight(repeat 1 n))
       (dotimes (i l-loop-dir)
           (setf xx(transpose (select nx (nth i loop-proj))))
           (setf m(length (nth i loop-proj)))
           (setf matx(make-array (list n m) :initial-contents xx))
           (setf dir(cdr (nth i loop-dir)))
           (setf p1x(%* matx dir))
           (setf w-out(sub-weight (min p1x)(max p1x) p1x tau))
           (setf loop-weight(* loop-weight w-out))
           )
      loop-weight))

  (defun sub-weight(a b px tau)
     (let*(
           (nob (length px))
           (d (* tau (- b a)))
           (d_1(repeat (- a d) nob))
           (d_2(repeat (+ b d) nob))
           (temp(+ (/ 1 (abs (- px d_1))) (/ 1 (abs (- px d_2)))))
           (h_t (exp (* (* d temp) -1)))
          )
      h_t))


  (defun sub-loc(case1 case2)
     (let*(
           (join (intersection case1 case2))
           (l_join (length join))
           (location(repeat 0 l_join))
           )
     (dotimes (i l_join)
           (setf (nth i location) (car (which (= case1 (nth i join)))))
           )
     (setf location(reverse location))
    location))


  (defun neighbor(c p1x tau)
     (let*(
        (min_x(min p1x))
        (max_x(max p1x))
        (cm  (which (< p1x c)))
;        (cmp (which (>= p1x (- c (* (- max_x c) tau)))))
        (cmp (which (>= p1x (- c (* (- c min_x) tau)))))
        (cm_nbr(intersection cm cmp))
        (cp  (which (>= p1x c)))
;        (cpm (which (< p1x (+ c (* (- c min_x) tau)))))
        (cpm (which (< p1x (+ c (* (- max_x c) tau)))))
        (cp_nbr(intersection cp cpm))
       )
     (list (list cm cp_nbr) (list cp cm_nbr))
     ))

  (defun neighbor1(x dir tau case proj-var)
     (let*(
          (n (length (car x)))
          (m (length proj-var))
           p1x
          (c (car dir))
          )
      (setf xx(transpose (select x proj-var)))
      (setf matx(make-array (list n m) :initial-contents xx))
      (setf p1x(%* matx (cdr dir)))
      (setf min_x(min p1x))
      (setf max_x(max p1x))
      (setf cm  (intersection case (which (< p1x c))))
;      (setf cmp (intersection case (which (>= p1x (- c (* (- max_x c) tau))))))
      (setf cmp (intersection case (which (>= p1x (- c (* (- c min_x) tau))))))
      (setf cm_nbr(intersection cm cmp))
      (setf cp  (intersection case (which (>= p1x c))))
;      (setf cpm (intersection case (which (< p1x (+ c (* (- c min_x) tau))))))
      (setf cpm (intersection case (which (< p1x (+ c (* (- max_x c) tau))))))
      (setf cp_nbr(intersection cp cpm))
     (list (list cm cp_nbr) (list cp cm_nbr))
     ))

  (defun neighbor2(x case proj-var dir loop-nbr nbr)
     (let*(
          (n (length (car x)))
          (m (length proj-var))
           nbr-set
           p1x
           ns
           )
      (if (= (length (intersection case (nth 1 (car nbr)))) 0)
           (setf nbr-set(nth 1 (car nbr)))
           (setf nbr-set(nth 1 (nth 1 nbr)))
           )
      (setf xx(transpose (select x proj-var)))
      (setf matx(make-array (list n m) :initial-contents xx))
      (setf p1x(%* matx (cdr dir)))
      (setf cm(intersection nbr-set (which (< p1x (car dir)))))
      (setf cp(intersection nbr-set (which (>= p1x (car dir)))))
      (setf ns(list (list (car (car loop-nbr))
                          (append (nth 1 (car loop-nbr)) cm))
                    (list (car (nth 1 loop-nbr))
                          (append (nth 1 (nth 1 loop-nbr)) cp))))
      ns))
