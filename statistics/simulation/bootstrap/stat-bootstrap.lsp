;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bootstrap procedures for stationary sequences. This
;; includes the moving blocks bootstrap of  Kuensch 
;; (Annals of Statistics, 17, 1989, 1217-1241) and 
;; Liu and Singh (in Exploring the Limits of Bootstrap,
;; New York, Wiley, 1992) and the stationary bootstrap 
;; of Politis and Romano (JASA, 89, 1984, 1303-1313).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stationary-bootstrap (data &key (prob .5))
"Args (data &key (prob .5)) 
Samples an element at random from data, then samples 
the next element with probability prob, or samples 
another random element with probability (1 - prob)."
(let* (
       (n (length data))
       (m (iseq n))
       (k (sample-one m))
       (b (list (elt data k)))
       )
(loop 
 (if (= n (length b)) (return b)
       (let* (
              (s (<= prob (random 1.0)))
              (l (if s (sample-one m) (+ k 1)))
              (i (if (>= l n) (sample-one m) l))
              (c (combine b (elt data i)))
              )
(setf k l) (setf b c)
)))
))

(defun moving-blocks-bootstrap (data &key (b 2))
"Args (data &key (b 2))
Partitions data into consecutive blocks of length
b. Then resamples with replacement from the blocks, 
and concatenates the resamples to create a boostrap
series"
(let* (
       (n (length data))
       (m (iseq (- n (1- b))))
       (k (ceiling (/ n b))) 
       (c nil)
       )
(loop
 (if (< n (length c)) (return (select c (iseq n)))
     (let* (
            (l (sample-one m))
            (p (+ l (1- b)))
            (q (iseq l p))
            )
     (setf c (append c (select data q))))))
))

(defun sample-one (data)
  (first (sample data 1))
)