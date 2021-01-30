;;;;
;;;;     STREAMS.LSP   Implementation of Abelson,Sussman, and Sussman
;;;;                   methods for handling streams.
;;;;
;;;;          22 Sep 92...Created to support iteration in simulations.
;;;;

(provide "streams")

;;;;
;;;;     Delay and force utilities
;;;;

(defun memoize-function (func)
  (let ((have-run? nil)
        (result    nil)  )
    (lambda () 
      (unless have-run?
              (setf have-run? t)
              (setf result (funcall func)))
      result)))

(defmacro delay (arg)
  `(memoize-function (lambda () ,arg)))

(defun force (arg)
  (funcall arg))

;;;;
;;;;     Streams
;;;;


(defmacro MAKE-STREAM (head tail)
  `(cons ,head (delay ,tail)))

(defun HEAD-STREAM (stream)
  (car stream))

(defun TAIL-STREAM (stream)
  (force (cdr stream)))

(setf THE-EMPTY-STREAM ())

(defun IS-EMPTY-STREAM? (s)
  (endp s))


(defun APPEND-STREAM (s1 s2)
  (if (is-empty-stream? s1)
      s2
      (make-stream (head-stream s1)
                   (append-stream (tail-stream s1) s2))))

(defun PRINT-STREAM (s)
  (if (is-empty-stream? s)
      (format t "~%")
      (progn
       (format t " ~a " (head-stream s))
       (print-stream (tail-stream s))
       )))

(defun FORCE-STREAM (s)
  "Traverse the stream forcing its evaluation."
  (if (is-empty-stream? s)
      nil
      (force-stream (tail-stream s))))


(defun MAKE-LIST-FROM-STREAM (s)
  (if (is-empty-stream? s)
      ()
      (cons (head-stream s)
            (make-list-from-stream (tail-stream s)))))

(defun FLATTEN-LIST-FROM-STREAM (s)
  (if (is-empty-stream? s)
      ()
      (append (head-stream s)
              (flatten-list-from-stream (tail-stream s)))))

;;;;
;;;;     Special strea
;;;;
;;;;     Special streams
;;;;

(defun INTEGER-STREAM (lo hi)
  (if (> lo hi)
      the-empty-stream
      (make-stream lo (integer-stream (1+ lo) hi))
      ))

(defun LIST-STREAM (list)
  (if (endp list)
      the-empty-stream
      (make-stream (first list)
                   (list-stream (rest list)))
      ))

;;;;
;;;;     Accumulation
;;;;

(defun ACCUMULATE-STREAM (func initial-value s)
  (if (is-empty-stream? s)
      initial-value
      (funcall func (head-stream s)
               (accumulate-stream func initial-value (tail-stream s)))
     ))

(defun LEFT-ACCUMULATE-STREAM (func initial-value s)
  (if (is-empty-stream? s)
      initial-value
      (accumulate-left-stream func
                              (funcall func initial-value (head-stream s))
                              (tail-stream s))
      ))

(defun ACCUMULATE-N-STREAMS (func init s)
  "Accumulate elements of stream of streams."
  (if (is-empty-stream? s)
      the-empty-stream
      (make-stream
       (accumulate-stream func init (map-stream #'head-stream s))
       (accumulate-n-streams func init (map-stream #'tail-stream s))
       )))


(defun SUM-STREAM (s)
 (accumulate-stream #'+ 0 s)) 
               
(defun PRODUCT-STREAM (s)
  (accumulate-stream #'* 1 s))

(defun CONS-STREAM (s)
  (accumulate-stream #'cons () s))


;;;;
;;;;     Maps and filters
;;;;

(defun FLATTEN-STREAM (stream-of-streams)
  (accumulate-stream #'append-stream the-empty-stream stream-of-streams))

(defun MAP-STREAM (func s)
  (if (is-empty-stream? s)
      the-empty-stream
      (make-stream (funcall func (head-stream s))
                   (map-stream func (tail-stream s)))
      ))

(defun MAP-PAIR-STREAM (func s1 s2)
  (if (is-empty-stream? s1)
      the-empty-stream
      (make-stream (funcall func (head-stream s1) (head-stream s2))
                   (map-pair-stream func (tail-stream s1) (tail-stream s2)))
      ))


(defun FLAT-MAP-STREAM (func s)
  (flatten-stream (map-stream func s)))

(defun FILTER-STREAM (pred s)
  (cond
    ((is-empty-stream? s) the-empty-stream)
    ((funcall pred (head-stream s))
                          (make-stream (head-stream s)
                                       (filter-stream pred (tail-stream s))))
    ( t                   (filter-stream pred (tail-stream s)))
    ))

(defun SELECT-STREAM (stream index)
  (if (numberp index)  ; force to be a list
      (first (select-stream stream (list index)))
      (let* ((ord (order index))
             (idx (select index ord))
             (max (first (last idx)))
             (result ())  )
        (dotimes (i (1+ max))
                 (when (= i (first idx))
                       (push (head-stream stream) result)
                       (setf idx (rest idx))  )
                 (setf stream (tail-stream stream))  )
        (select result (reverse ord)))))
                 
        
  



#||     Testing

(def is (integer-stream 0 10))
(sum-stream is)

(flat-map-stream #'(lambda(i)
                     (map-stream #'(lambda (j) (list i j))
                                 (integer-stream 1 (- i 1))))
                 (integer-stream 1 10))

||#
    
  