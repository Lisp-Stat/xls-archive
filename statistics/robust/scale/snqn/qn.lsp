(defun qn-estimate (x)
 (let* (
        (x (sort-data x))
        (n (length x))
        (left (repeat nil n))
        (right (repeat n n))
        (weight (repeat nil n))
        (work (repeat nil n))
        (q (repeat nil n))
        (p (repeat nil n))
        (h (floor (1+ (/ n 2))))
        (k (floor (* h (/ (1- h) 2))))
        (jhelp (floor (* n (/ (1+ n) 2))))
        (knew (+ k jhelp))
        (nr (^ n 2))
        (nl jhelp)
        (found nil)
        (trial nil)
        (sump 0)
        (sumq 0)
        (j 1)
       )
    (dolist (i (iseq 1 n))
     (setf (elt left (1- i)) (- (+ n 2) i))
    )
  (loop 
    (cond ((and (> (- nr nl) n) (not found))
             (setf j 1)
             (dolist (i (iseq 2 n))
               (cond ((<= (elt left (1- i)) (elt right (1- i)))
                      (setf (elt weight (1- j))
                            (1+ (- (elt right (1- i)) (elt left (1- i)))))
                      (setf jhelp (floor (+ (elt left (1- i))
                                            (/ (elt weight (1- j)) 2))))
                      (setf (elt work (1- j)) (- (elt x (1- i))
                                                 (elt x (- n jhelp))))
                      (setf j (1+ j))
                     )
               )
             )
             (setf trial (whimed work weight (1- j)))
             (setf j 0)
             (dolist (i (iseq n 1))
              (loop
                (cond ((and (< j n) 
                           (< (- (elt x (1- i)) (elt x (- n j 1))) trial))
                        (setf j (1+ j)))
                      (t (return))
                )
              )
               (setf (elt p (1- i)) j)
             )
             (setf j (1+ n))
             (dolist (i (iseq 1 n))
               (loop 
                 (cond ((> (- (elt x (1- i)) (elt x (1+ (- n j)))) trial)
                        (setf j (1- j)))
                       (t (return))
                 )
               )
               (setf (elt q (1- i)) j)
             )
             (setf sump (sum p))
             (setf sumq (sum (1- q)))
             (cond ((<= knew sump)
                    (setf right (copy-list p))
                    (setf nr sump))
   
                   ((> knew sumq)
                    (setf left (copy-list q))
                    (setf nl sumq))
   
                   (t 
                     (setf qn trial)
                     (setf found t))
 
             )
          )
          (t (return))
  ))

  (cond ((not found)
         (setf j 1)
         (dolist (i (iseq 2 n))
           (if (<= (elt left (1- i)) (elt right (1- i)))
               (dolist (jj (iseq (elt left (1- i)) (elt right (1- i))))
                  (setf (elt work (1- j)) (- (elt x (1- i))
                                             (elt x (- n jj))))
                  (setf j (1+ j))
               )
           )
         )
        (setf qn (elt (sort-data (select work (iseq (1- j)))) (- knew nl 1)))))
  (setf dn (cond ((<= n 9) 
                  (case n (2 .399)
                          (3 .994)
                          (4 .512)
                          (5 .844)
                          (6 .611)
                          (7 .857)
                          (8 .669)
                          (9 .872)))
                 (t 
                    (cond ((= (mod n 2) 1) (/ n (+ n 1.4)))
                           (t (/ n (+ n 3.8)))))))


  (* 2.2219 dn qn)
 )
)



(defun whimed (a iw n)
 (let* ( 
        (acand (repeat nil n))
        (iwcand (repeat nil n))
        (wtotal (floor (sum (select iw (iseq n)))))
        (wrest 0)
        (wleft 0)
        (wmid 0)
        (wright 0)
        (nn n)
        (trial nil)
        (kcand 0)
       )
    (loop 
      (setf trial (elt (sort-data (select a (iseq nn)))
                    (floor (/ nn 2))))
      (setf wleft 0)
      (setf wmid 0)
      (setf wright 0)
      (dotimes (i nn)
        (cond ((< (elt a i) trial)
               (setf wleft (+ wleft (floor (elt iw i)))))
              ((> (elt a i) trial)
                       (setf wright (+ wright (floor (elt iw i)))))
              (t 
                (setf wmid (+ wmid (floor (elt iw i))))))
      )
      (cond ((> (* 2 (+ wrest wleft)) wtotal)
             (setf kcand 0)
             (dotimes (i nn)
              (cond ((< (elt a i) trial)
                     (setf kcand (1+ kcand))
                     (setf (elt acand (1- kcand)) (elt a i))
                     (setf (elt iwcand (1- kcand)) (elt iw i))))
             )
             (setf nn kcand))
            ((> (* 2 (+ wrest wleft wmid)) wtotal)
             (return))
            (t 
              (setf kcand 0)
              (dotimes (i nn)
               (cond ((> (elt a i) trial)
                      (setf kcand (1+ kcand))
                      (setf (elt acand (1- kcand)) (elt a i))
                      (setf (elt iwcand (1- kcand)) (elt iw i))))
              )
              (setf nn kcand)
              (setf wrest (+ wrest wleft wmid))
            )
      )
      (dotimes (i nn)
       (setf (elt a i) (elt acand i))
       (setf (elt iw i) (elt iwcand i)))
    )
  trial
 )
)


