#|

From: ethan+@pitt.edu (Ethan Banatan)

One of the things that I love about Lisp is that encourages recursion.
It may not be efficient in every case but it is elegantly simple and
fun to write!

Here's a recursive solution (I've written it to be easy to understand
rather than compact or efficient):

|#

(defun permute (list)
  (if (= 1 (length list))
    (list list)
    (do ((n 0 (1+ n))
         (permuted-list '())
         item
         sublist)
        ((= n (length list)) permuted-list)

      ; remove each item, one at a time,  and tack it on the front of
      ; every permutation of the remaining items.
      (setf item (nth n list)
            sublist (append (subseq list 0 n)
                            (subseq list (1+ n))))

      (dolist (permuted-sublist (permute sublist))
        (push (append (list item) permuted-sublist)
              permuted-list)))))

#|

I added this for the case in which there are duplicate entities in the
list.

Jan de Leeuw (deleeuw@stat.ucla.edu)

|#

(defun permute-without-duplicates (list)
	(delete-duplicates (permute list) :test #'equal))



