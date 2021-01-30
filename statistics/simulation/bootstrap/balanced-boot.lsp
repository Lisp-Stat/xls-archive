; see the paper
;
;     Algorithms for Balanced Bootstrap Simulations
;     The American Statistician, November, 1988, Vol 42, No 4
;

(defun simple-bootstrap (the-sample)
       (sample the-sample (length the-sample) t))

; obs-list - the observations which will be sampled, in vector form for rapid
;            lookup
; aux-list - the number of observations to be looked up, in vector form
;                   for rapid lookup
; bin-count - the number of bins left (ie, the number of samples which have not
;             been all taken).
; max-bin-val - the maximum number of observations left in any bin
; max-bin-idx - the index to the bin with the most observations left

(defproto balanced-bootstrap
          '(obs-list list-len bin-count aux-list max-bin-val max-bin-idx
            boot-count n-of-boots max-boots))

(defmeth balanced-bootstrap :isnew (the-obs-list n-of-boots)
    (send self :slot-value 'obs-list (concatenate 'vector the-obs-list))
    (send self :slot-value 'list-len (length (slot-value 'obs-list)))
    (send self :slot-value 'bin-count (slot-value 'list-len))
    (send self :slot-value 'aux-list
          (concatenate 'vector (repeat n-of-boots (slot-value 'bin-count))))
    (send self :slot-value 'max-bin-val n-of-boots)
    (send self :slot-value 'max-bin-idx 0)
    (send self :slot-value 'boot-count 0)
    (send self :slot-value 'max-boots n-of-boots)
)

(defmeth balanced-bootstrap :next-bootstrap ()
  (cond
    ((>= (slot-value 'boot-count) (slot-value 'max-boots)) nil)
    (t
      (setf (slot-value 'boot-count) (1+ (slot-value 'boot-count)))

      (do ((count (slot-value 'list-len) (setq count (1- count)))
           (new-sample nil)
           (next-idx nil)
           (random-test nil))
        ((= count 0) (return new-sample))

        ; select 1 element of a sample
        (loop
          (setf next-idx (random (slot-value 'bin-count)))
          (setf random-test (random (slot-value 'max-bin-val)))
          (if (< random-test (elt (slot-value 'aux-list) next-idx))
              (return))
        )

        ; add the selected element to the list of elements
        (setf new-sample
              (cons (elt (slot-value 'obs-list) next-idx) new-sample))

        ; decrement the selected bin
        (setf (elt (slot-value 'aux-list) next-idx)
              (- (elt (slot-value 'aux-list) next-idx) 1))

        ; if we've selected from the most full slot, find the next most full
        ; slot
        (if (and (= next-idx (slot-value 'max-bin-idx))
                 (> (- (slot-value 'max-bin-val)
                       (elt (slot-value 'aux-list)
                            (slot-value 'max-bin-idx)))
                    (floor (/ (+ (slot-value 'max-boots)
                                 (- (slot-value 'boot-count))
                                 (slot-value 'list-len))
                              (slot-value 'list-len)))))
            (do ((i 0 (setf i (1+ i))))
                ((= i (slot-value 'bin-count)) nil)
                (cond ((> (elt (slot-value 'obs-list) i)
                          (slot-value 'max-bin-val))
                       (slot-value 'max-bin-idx i)
                       (slot-value 'max-bin-val (elt (slot-value 'obs-list) i))
                      )
                )
            )
        )

        ; if we've emptied a bin, place it at the end of the list and shorten
        ; the list so it is excluded from the search on future passes
        (cond ((= (elt (slot-value 'aux-list) next-idx) 0)
               (if (= (slot-value 'max-bin-idx) (slot-value 'bin-count))
                   (setf (slot-value 'max-bin-idx) (slot-value 'bin-count)))
               (setf (elt (slot-value 'aux-list) next-idx)
                     (elt (slot-value 'aux-list) (1- (slot-value 'bin-count))))
               (setf (elt (slot-value 'obs-list) next-idx)
                     (elt (slot-value 'obs-list) (1- (slot-value 'bin-count))))
               (setf (slot-value 'bin-count) (1- (slot-value 'bin-count)))
              )
        )
      )
    )
  )
)
 
(defun balanced-bootstrap (the-sample n-of-boots the-function)
(let (
      (bb (send balanced-bootstrap :new the-sample n-of-boots))
      (gg (repeat nil n-of-boots))
      )
(dotimes (i n-of-boots gg)
(setf (elt gg i) (funcall the-function (send bb :next-bootstrap))))
))

